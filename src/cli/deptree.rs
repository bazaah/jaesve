use {crate::models::field::Field, std::collections::HashMap};

const FIELDS: [Field; 7] = [
    Field::Delimiter,
    Field::Guard,
    Field::Identifier,
    Field::JmesPath,
    Field::Pointer,
    Field::Type,
    Field::Value,
];

/// There are two variants of dependency that need to be mapped:
/// 1. Needs to be calculated
/// 2. Needs to be stored (which implies 1)
///
/// In the absence of a better idea, I currently store them as HashMap<Field, bool>
/// Thus if a field exists in the map it is at least 1, if its value is true it is also 2
pub(in crate::cli) struct DependencyTree {
    /// Field and its dependencies, if any
    map: HashMap<Field, Option<Vec<Field>>>,
}

impl DependencyTree {
    /// Initialize the base relations between Fields
    pub(in crate::cli) fn init() -> Self {
        let mut map = HashMap::with_capacity(FIELDS.len());
        FIELDS
            .iter()
            .map(|field| match field {
                f @ Field::Delimiter => (f, None),
                f @ Field::Guard => (f, None),
                f @ Field::Identifier => (f, Some(vec![Field::Guard, Field::Delimiter])),
                f @ Field::JmesPath => (f, Some(vec![Field::Pointer])),
                f @ Field::Pointer => (f, Some(vec![Field::Value])),
                f @ Field::Type => (f, Some(vec![Field::Value])),
                f @ Field::Value => (f, Some(vec![Field::Guard, Field::Delimiter])),
            })
            .for_each(|(field, dependencies)| {
                map.insert(*field, dependencies);
            });
        DependencyTree { map }
    }
    /// Generate a dependency map, used for determining what work this instance of the program needs to do
    pub(in crate::cli) fn generate_list<F: AsRef<[Field]>>(
        &self,
        relevant: F,
    ) -> HashMap<Field, bool> {
        let mut set = HashMap::<Field, bool>::with_capacity(FIELDS.len());
        // Populate the dependency map with with all secondary Fields
        relevant
            .as_ref()
            .iter()
            .scan(Vec::<(Field, bool)>::new(), |buffer, field| {
                match self.map.get(field).unwrap() {
                    Some(deps) => buffer.extend(deps.iter().map(|f| (*f, false))),
                    None => {}
                }

                buffer.pop()
            })
            .for_each(|(field, is_output)| {
                set.entry(field).or_insert(is_output);
            });
        // Populate the dependency map with the primary Fields, overwriting secondary values
        relevant
            .as_ref()
            .iter()
            .map(|field| (*field, true))
            .for_each(|(field, is_output)| {
                set.insert(field, is_output);
            });

        set
    }
}
