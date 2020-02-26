use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InputCellID(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ComputeCellID(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CallbackID(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CellID {
    Input(InputCellID),
    Compute(ComputeCellID),
}

#[derive(Debug, PartialEq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

struct ComputeCell<'a, T> {
    dependencies: Vec<CellID>,
    compute_func: Box<dyn 'a + Fn(&[T]) -> T>,
}

pub struct Reactor<'a, T> {
    input_cells: HashMap<InputCellID, T>,
    compute_cells: HashMap<ComputeCellID, ComputeCell<'a, T>>,
    compute_callback_ids: HashMap<ComputeCellID, Vec<CallbackID>>,
    callbacks: HashMap<CallbackID, Box<dyn 'a + FnMut(T) -> ()>>,
    dependencies: HashMap<CellID, Vec<ComputeCellID>>,
}

impl<'a, T: Copy + PartialEq> Reactor<'a, T> {
    pub fn new() -> Self {
        Self {
            input_cells: HashMap::new(),
            compute_cells: HashMap::new(),
            compute_callback_ids: HashMap::new(),
            callbacks: HashMap::new(),
            dependencies: HashMap::new(),
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
        let compute_cell_id = ComputeCellID(self.compute_cells.len());
        for dependency in dependencies {
            if let None = self.value(*dependency) {
                return Err(*dependency);
            } else {
                self.dependencies
                    .entry(*dependency)
                    .or_insert(vec![])
                    .push(compute_cell_id);
            }
        }
        let compute_cell = ComputeCell {
            dependencies: dependencies.to_vec(),
            compute_func: Box::new(compute_func),
        };
        self.compute_cells.insert(compute_cell_id, compute_cell);
        Ok(compute_cell_id)
    }

    pub fn value(&self, id: CellID) -> Option<T> {
        match id {
            CellID::Input(id) => self.input_cells.get(&id).cloned(),
            CellID::Compute(id) => self.compute_value(id),
        }
    }

    fn compute_value(&self, id: ComputeCellID) -> Option<T> {
        if let Some(cell) = self.compute_cells.get(&id) {
            let dependencies: Vec<_> = cell
                .dependencies
                .iter()
                .filter_map(|&d| self.value(d))
                .collect();
            Some((cell.compute_func)(&dependencies))
        } else {
            None
        }
    }

    pub fn set_value(&mut self, id: InputCellID, new_value: T) -> bool {
        match self.input_cells.get_mut(&id) {
            Some(v) => {
                if v != &new_value {
                    *(v) = new_value;
                }

                let dependencies = self
                    .dependencies
                    .get(&CellID::Input(id))
                    .map_or(vec![], |d| {
                        d.iter()
                            .map(|&d| (d, self.compute_value(d).unwrap()))
                            .collect()
                    });

                for (compute_cell_id, value) in dependencies.into_iter() {
                    let callback_ids = self
                        .compute_callback_ids
                        .entry(compute_cell_id)
                        .or_default();
                    for callback_id in callback_ids {
                        let callback = self.callbacks.get_mut(&callback_id).unwrap();
                        callback(value);
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
        self.compute_callback_ids
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
        let compute_callbacks = self.compute_callback_ids.get_mut(&cell).unwrap();
        let callback_index = compute_callbacks
            .iter()
            .position(|c| *c == callback)
            .unwrap();
        compute_callbacks.remove(callback_index);
        Ok(())
    }
}
