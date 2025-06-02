// Now this becomes a simple facade
use crate::cfg::CfgProgram;
use crate::verification::boogie_codegen::BoogieCodeGenerator;
use crate::verification::verification_logic::VerificationPlan;
use crate::verification::VerificationUnit;

/// Main entry point for Boogie code generation
pub fn generate_verification_boogie_code(unit: &VerificationUnit, cfg: &CfgProgram) -> String {
    // 1. Build verification plan (logic)
    let plan = VerificationPlan::from_verification_unit(unit, cfg);

    // 2. Generate Boogie code (codegen)
    let codegen = BoogieCodeGenerator::new();
    codegen.generate_code(&plan, cfg)
}
