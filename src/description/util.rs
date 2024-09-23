use std::fmt::Write;

pub trait IterUtil: Iterator {
    // based on Itertools::join
    fn join(&mut self, sep: &str) -> String
    where
        Self::Item: std::fmt::Display,
    {
        match self.next() {
            None => String::new(),
            Some(first_elt) => {
                // estimate lower bound of capacity needed
                let (lower, _) = self.size_hint();
                let mut result = String::with_capacity(sep.len() * lower);
                write!(&mut result, "{}", first_elt).unwrap();
                self.for_each(|elt| {
                    result.push_str(sep);
                    write!(&mut result, "{}", elt).unwrap();
                });
                result
            }
        }
    }

    fn find_duplicate(&mut self) -> Option<Self::Item>
    where
        Self::Item: PartialEq,
    {
        let (lower, _) = self.size_hint();
        let mut checked = Vec::with_capacity(lower);

        for elt in self {
            if checked.contains(&elt) {
                return Some(elt);
            }
            checked.push(elt);
        }

        None
    }
}

impl<T> IterUtil for T where T: Iterator {}
