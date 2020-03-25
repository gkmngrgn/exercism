// This was the hardest puzzle on Exercism for me. I couldn't passed all the
// unit tests so I got help from another solution. The original solution is
// here:
//
// https://exercism.io/tracks/rust/exercises/react/solutions/296fb2da2de74b8b96e1d3b7f467ae04
//
// Note for me: The another way is to use Cell and RefCell instead of Rc.
use std::rc::Rc;

/// `InputCellID` is a unique identifier for an input cell.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InputCellID(usize);

/// `ComputeCellID` is a unique identifier for a compute cell.
/// Values of type `InputCellID` and `ComputeCellID` should not be mutually assignable,
/// demonstrated by the following tests:
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input: react::ComputeCellID = r.create_input(111);
/// ```
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input = r.create_input(111);
/// let compute: react::InputCellID = r.create_compute(&[react::CellID::Input(input)], |_| 222).unwrap();
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ComputeCellID(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CallbackID(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CellID {
    Input(InputCellID),
    Compute(ComputeCellID),
}

impl From<CellID> for usize {
    fn from(id: CellID) -> Self {
        match id {
            CellID::Input(InputCellID(i)) => i,
            CellID::Compute(ComputeCellID(i)) => i,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

struct InputCell<T> {
    parents: Rc<Vec<ComputeCellID>>,
    value: T,
}

type Callback<'a, T> = Box<dyn FnMut(T) + 'a>;

struct ComputeCell<'a, T> {
    parents: Rc<Vec<ComputeCellID>>,
    dependencies: Vec<CellID>,
    callbacks: Vec<Option<Callback<'a, T>>>,
    compute_func: Box<dyn Fn(&[T]) -> T + 'a>,
    value: T,
}

impl<'a, T> ComputeCell<'a, T> {
    fn value(&self, cells: &[Cell<T>]) -> Option<T>
    where
        T: Copy,
    {
        let dependencies: Option<Vec<T>> = self
            .dependencies
            .iter()
            .map(|id| {
                cells.get(usize::from(*id)).and_then(|cell| match cell {
                    Cell::Input(c) => Some(c.value),
                    Cell::Compute(c) => c.value(cells),
                })
            })
            .collect();
        Some((self.compute_func)(&dependencies?))
    }
}

enum Cell<'a, T> {
    Input(InputCell<T>),
    Compute(ComputeCell<'a, T>),
}

#[derive(Default)]
pub struct Reactor<'a, T> {
    cells: Vec<Cell<'a, T>>,
}

impl<'a, T: Copy + PartialEq> Reactor<'a, T> {
    pub fn new() -> Self {
        Self { cells: vec![] }
    }

    pub fn create_input(&mut self, initial: T) -> InputCellID {
        let input_cell_id = InputCellID(self.cells.len());
        let input_cell = InputCell {
            parents: Rc::new(vec![]),
            value: initial,
        };
        self.cells.push(Cell::Input(input_cell));
        input_cell_id
    }

    pub fn create_compute<F: Fn(&[T]) -> T + 'a>(
        &mut self,
        dependencies: &[CellID],
        compute_func: F,
    ) -> Result<ComputeCellID, CellID> {
        let compute_cell_id = ComputeCellID(self.cells.len());
        let dependencies = dependencies
            .iter()
            .map(|&d| {
                if usize::from(d) < self.cells.len() {
                    Ok(d)
                } else {
                    Err(d)
                }
            })
            .collect::<Result<Vec<CellID>, CellID>>()?;

        for &dep in &dependencies {
            let cell = self.cells.get_mut(usize::from(dep)).ok_or(dep)?;
            let parents = match cell {
                Cell::Input(c) => &mut c.parents,
                Cell::Compute(c) => &mut c.parents,
            };
            Rc::get_mut(parents).unwrap().push(compute_cell_id);
        }

        let value = {
            let dependencies: Result<Vec<T>, CellID> = dependencies
                .iter()
                .map(|&id| {
                    self.cells
                        .get(usize::from(id))
                        .and_then(|cell| match cell {
                            Cell::Input(c) => Some(c.value),
                            Cell::Compute(c) => c.value(&self.cells),
                        })
                        .ok_or(id)
                })
                .collect();
            (compute_func)(&dependencies?)
        };

        let compute_cell = ComputeCell {
            parents: Rc::new(vec![]),
            dependencies,
            callbacks: vec![],
            compute_func: Box::new(compute_func),
            value,
        };
        self.cells.push(Cell::Compute(compute_cell));
        Ok(compute_cell_id)
    }

    pub fn value(&self, id: CellID) -> Option<T> {
        let cell = self.cells.get(usize::from(id))?;
        match cell {
            Cell::Input(c) => Some(c.value),
            Cell::Compute(c) => c.value(&self.cells),
        }
    }

    pub fn set_value(&mut self, id: InputCellID, new_value: T) -> bool {
        match self.cells.get_mut(id.0) {
            Some(Cell::Input(cell)) => {
                (*cell).value = new_value;
                let parents = Rc::clone(&cell.parents);
                self.notify(&parents);
                true
            }
            _ => false,
        }
    }

    fn notify(&mut self, parents: &[ComputeCellID]) -> Option<()> {
        for parent in parents {
            let new_value = match self.cells.get(parent.0)? {
                Cell::Input(c) => c.value,
                Cell::Compute(c) => c.value(&self.cells)?,
            };

            if let Cell::Compute(ComputeCell {
                value,
                callbacks,
                parents,
                ..
            }) = self.cells.get_mut(parent.0)?
            {
                let parents = Rc::clone(parents);
                if new_value != *value {
                    *value = new_value;
                    callbacks.iter_mut().for_each(|callback| {
                        if let Some(callback) = callback {
                            (callback)(new_value);
                        }
                    })
                }
                self.notify(&parents);
            }
        }

        Some(())
    }

    pub fn add_callback<F: FnMut(T) -> () + 'a>(
        &mut self,
        id: ComputeCellID,
        callback: F,
    ) -> Option<CallbackID> {
        match self.cells.get_mut(id.0)? {
            Cell::Compute(c) => {
                let callback_id = CallbackID(c.callbacks.len());
                c.callbacks.push(Some(Box::new(callback)));
                Some(callback_id)
            }
            _ => None,
        }
    }

    pub fn remove_callback(
        &mut self,
        cell: ComputeCellID,
        callback: CallbackID,
    ) -> Result<(), RemoveCallbackError> {
        if let Cell::Compute(c) = self
            .cells
            .get_mut(cell.0)
            .ok_or(RemoveCallbackError::NonexistentCell)?
        {
            let callback = c
                .callbacks
                .get_mut(callback.0)
                .filter(|cb| cb.is_some())
                .ok_or(RemoveCallbackError::NonexistentCallback)?;
            *callback = None;
        }
        Ok(())
    }
}
