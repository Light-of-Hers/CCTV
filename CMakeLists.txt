cmake_minimum_required(VERSION 3.15)
project(CCTV)

set(CMAKE_CXX_STANDARD 17)

include_directories(.)
include_directories(/usr/lib/llvm-9/include/c++/v1)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
set(CMAKE_EXE_LINKER_FLAGS "-lc++abi")

add_executable(CCTV
        crz_lisp.hh
        example.cc)
