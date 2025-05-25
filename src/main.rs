mod ast;
mod parser;
mod printer;
mod semantic;
mod errors;

use crate::parser::parse_program;
use crate::semantic::SemanticAnalyzer;
use crate::errors::format_errors;
use crate::printer::{print_program, PrintOptions, PrintMode};

fn main() {
    let source_code: &'static str = r#"
nodes {
    NodeA,
    NodeB
}

table Users on NodeA {
    int userID;
    bool isActive;
    float balance;
}

table Orders on NodeB {
    int orderID;
    float amount;
    bool isPaid;
}

int calculateDiscount(int points, bool isPremium) {
    hop on NodeA {
        int discount = 0;
        float rate = 0.1;
        
        if (isPremium == true) {
            discount = points * 2;
        } else {
            discount = points;
        }
        
        return discount;
    }
}

void processPayment(int userID, int orderID) {
    hop on NodeA {
        float userBalance = Users[userID: userID].balance;
        bool isActiveUser = Users[userID: userID].isActive;
    }
    
    hop on NodeB {
        float orderAmount = Orders[orderID: orderID].amount;
        Orders[orderID: orderID].isPaid = true;
        return;
    }
}

int invalidCrossNodeAccess(int userID) {
    hop on NodeA {
        // This should cause an error: trying to access Users table (on NodeA) from NodeB
        float balance = Users[userID: userID].balance;
        return 0;
    }
}
"#;

    match parse_program(source_code) {
        Ok(program) => {
            println!("🎉 Parse successful!\n");
            
            print_program(&program, &PrintOptions {
                mode: PrintMode::Verbose,
                show_spans: true,
            });
            
            println!("\n{}\n", "=".repeat(50));
            
            let mut analyzer = SemanticAnalyzer::new();
            match analyzer.analyze(&program) {
                Ok(()) => println!("✅ Analysis passed!"),
                Err(errors) => {
                    println!("❌ Semantic analysis failed!\n");
                    println!("{}", format_errors(&errors, source_code));
                }
            }
        }
        Err(errors) => {
            println!("❌ Parse failed!\n");
            println!("{}", format_errors(&errors, source_code));
        }
    }
}
