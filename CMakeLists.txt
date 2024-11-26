cmake_minimum_required(VERSION 3.25)
project(untitled1)

set(CMAKE_CXX_STANDARD 23)


#add_executable(untitled1 svm.cpp svm_parser.cpp svm_run.cpp)
add_executable(untitled1 exp.cpp parser.cpp environment.hh imp_codegen.cpp  imp_type_checker.cpp scanner.cpp token.cpp  visitor.cpp main.cpp imp_type.cpp imp_interpreter.cpp imp_value.cpp imp_value_visitor.hh)
