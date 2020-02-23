use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub struct InputCellID(usize);

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub struct ComputeCellID(usize);

pub struct ComputeCell<'a, T> {
    dependencies: Vec<CellID>,
    compute_func: Box<dyn 'a + Fn(&[T]) -> T>,
    value: T,
}

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub struct CallbackID(usize);

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub enum CellID {
    Input(InputCellID),
    Compute(ComputeCellID),
}

#[derive(Debug, PartialEq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

pub struct Reactor<'a, T> {
    input_cells: HashMap<InputCellID, T>,
    compute_cells: HashMap<ComputeCellID, ComputeCell<'a, T>>,
    compute_callbacks: HashMap<ComputeCellID, Vec<CallbackID>>,
    callbacks: HashMap<CallbackID, Box<dyn 'a + FnMut(T) -> ()>>,
}

impl<'a, T: Copy + PartialEq> Reactor<'a, T> {
    pub fn new() -> Self {
        Self {
            input_cells: HashMap::new(),
            compute_cells: HashMap::new(),
            compute_callbacks: HashMap::new(),
            callbacks: HashMap::new(),
        }
    }

    pub fn create_input(&mut self, initial: T) -> InputCellID {
        let input_cell_id = InputCellID(self.input_cells.len());
        self.input_cells.insert(input_cell_id, initial);
        input_cell_id
    }

    pub fn create_compute<F: 'a + Fn(&[T]) -> T>(
        &mut self,
        dependencies: &[CellID],
        compute_func: F,
    ) -> Result<ComputeCellID, CellID> {
        let values = dependencies
            .iter()
            .map(|&id| match self.value(id) {
                Some(value) => Ok(value),
                None => Err(id),
            })
            .collect::<Result<Vec<T>, CellID>>()?;
        let compute_cell_id = ComputeCellID(self.compute_cells.len());
        let compute_cell = ComputeCell {
            dependencies: dependencies.to_vec(),
            compute_func: Box::new(compute_func),
            value: compute_func(&values),
        };
        self.compute_cells.insert(compute_cell_id, compute_cell);
        Ok(compute_cell_id)
    }

    pub fn value(&self, id: CellID) -> Option<T> {
        match id {
            CellID::Input(id) => self.input_cells.get(&id),
            CellID::Compute(id) => match self.compute_cells.get(&id) {
                Some(cell) => Some(&cell.value),
                _ => None,
            },
        }
        .cloned()
    }

    pub fn set_value(&mut self, id: InputCellID, new_value: T) -> bool {
        match self.input_cells.get_mut(&id) {
            Some(v) => {
                if v != &new_value {
                    *(v) = new_value;
                    for cell in self
                        .compute_cells
                        .values_mut()
                        .filter(|cell| cell.dependencies.contains(&CellID::Input(id)))
                    {
                        cell.value = new_value // Nope, it's not true..
                    }
                }
                true
            }
            None => false,
        }
    }

    pub fn add_callback<F: 'a + FnMut(T) -> ()>(
        &mut self,
        id: ComputeCellID,
        callback: F,
    ) -> Option<CallbackID> {
        if !self.compute_cells.contains_key(&id) {
            return None;
        }
        let callback_id = CallbackID(self.callbacks.len());
        self.callbacks.insert(callback_id, Box::new(callback));
        self.compute_callbacks
            .entry(id)
            .or_insert(vec![])
            .push(callback_id);
        Some(callback_id)
    }

    pub fn remove_callback(
        &mut self,
        cell: ComputeCellID,
        callback: CallbackID,
    ) -> Result<(), RemoveCallbackError> {
        if !self.compute_cells.contains_key(&cell) {
            return Err(RemoveCallbackError::NonexistentCell);
        }
        if !self.callbacks.contains_key(&callback) {
            return Err(RemoveCallbackError::NonexistentCallback);
        }
        self.callbacks.remove(&callback);
        let compute_callbacks = self.compute_callbacks.get_mut(&cell).unwrap();
        let callback_index = compute_callbacks
            .iter()
            .position(|c| *c == callback)
            .unwrap();
        compute_callbacks.remove(callback_index);
        Ok(())
    }
}
