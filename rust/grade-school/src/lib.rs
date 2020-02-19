use std::collections::HashMap;

pub struct School {
    grades_with_students: HashMap<u32, Vec<String>>,
}

impl School {
    pub fn new() -> School {
        Self {
            grades_with_students: HashMap::new(),
        }
    }

    pub fn add(&mut self, grade: u32, student: &str) {
        let mut students = match self.grade(grade) {
            Some(students) => students,
            None => Vec::new(),
        };
        students.push(student.to_string());
        students.sort();
        self.grades_with_students.insert(grade, students);
    }

    pub fn grades(&self) -> Vec<u32> {
        let mut grades: Vec<u32> = self.grades_with_students.keys().map(|&x| x).collect();
        grades.sort();
        grades
    }

    pub fn grade(&self, grade: u32) -> Option<Vec<String>> {
        match self.grades_with_students.get(&grade) {
            Some(students) => Some(students.to_vec()),
            None => None,
        }
    }
}
