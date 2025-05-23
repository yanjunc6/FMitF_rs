mod ast;
mod parser;
mod printer;
mod semantic;

use crate::parser::parse_program;
use crate::printer::print_program;
use crate::semantic::{format_semantic_errors, SemanticAnalyzer};

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

// Test basic function with variables and if-else
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

// Test void function and multiple hops
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

// Test global variables across hops
bool validateTransaction(int amount, bool urgent) {
    hop on NodeA {
        global int totalChecked = 0;
        global bool hasIssues = false;
        
        int threshold = 100;
        float fee = 2.5;
        
        // Test arithmetic and comparison
        bool expensive = (amount + fee) > threshold;
        totalChecked = totalChecked + 1;
        
        if (expensive == true) {
            hasIssues = true;
        }
    }
    
    hop on NodeB {
        // Global variables are accessible here
        // Local variables from previous hop are NOT accessible
        
        bool affordable = amount <= 1000;
        bool valid = false;
        
        if (hasIssues == false && affordable == true) {
            valid = true;
        } else {
            valid = false;
        }
        
        totalChecked = totalChecked + 1;
        return valid;
    }
}

// Test complex global variable usage
int processMultipleOrders(int userID) {
    hop on NodeA {
        global int processedCount = 0;
        global float totalAmount = 0.0;
        
        bool userActive = Users[userID: userID].isActive;
        if (userActive == true) {
            processedCount = 1;
            totalAmount = Users[userID: userID].balance;
        }
    }
    
    hop on NodeB {
        // Can access global variables from previous hop
        int orderID = userID + 100;
        float orderAmount = Orders[orderID: orderID].amount;
        
        if (orderAmount > 0.0) {
            totalAmount = totalAmount + orderAmount;
            processedCount = processedCount + 1;
            Orders[orderID: orderID].isPaid = true;
        }
        
        return processedCount;
    }
}
"#;

    println!("🚀 Testing TransAct Language Features");
    println!("====================================");

    match std::panic::catch_unwind(|| parse_program(source_code)) {
        Ok(program) => {
            println!("✅ Parsing successful!");

            let mut analyzer = SemanticAnalyzer::new();
            match analyzer.analyze(&program) {
                Ok(()) => {
                    println!("✅ Semantic analysis passed!");
                    println!("\n📊 Summary:");
                    println!("  - Nodes: {}", program.nodes.len());
                    println!("  - Tables: {}", program.tables.len());
                    println!("  - Functions: {}", program.functions.len());

                    println!("\n🔍 AST Structure:");
                    print_program(&program);
                }
                Err(errors) => {
                    println!("❌ Semantic errors ({}): ", errors.len());
                    println!("{}", format_semantic_errors(&errors));
                }
            }
        }
        Err(_) => {
            println!("💥 Parsing failed!");
        }
    }
}
