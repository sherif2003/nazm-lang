use crate::*;

impl<'a> SemanticsAnalyzer<'a> {
    pub(crate) fn analyze_consts(&mut self) {
        for const_key in self.ast.consts.keys() {
            self.analyze_const(const_key);
        }
    }

    fn analyze_const(&mut self, const_key: ConstKey) {
        if self.typed_ast.consts.contains_key(&const_key) {
            // The const is computed already
            return;
        } else if self.semantics_stack.consts.contains_key(&const_key) {
            // TODO: Cycle detected
            self.semantics_stack.is_cycle_detected = CycleDetected::Const(const_key);
            panic!("Cycle detected");
            return;
        }

        self.semantics_stack.consts.insert(const_key, ());
        let c = &self.ast.consts[const_key];
        // TODO
        self.semantics_stack.consts.remove(&const_key);
    }
}
