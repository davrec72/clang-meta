set(LLVM_LINK_COMPONENTS support)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)




add_custom_command(OUTPUT
        "${CMAKE_CURRENT_SOURCE_DIR}/generated_files/reflection_incs/ReflectionHeaderID.h"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/GenReflectionSrc.cpp"
                "${CLANG_SOURCE_DIR}/include/clang/AST/ReflectionIncludes.hpp"
        COMMAND ${CMAKE_COMMAND} "-DTHE_CURSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}" -P
                "${CMAKE_CURRENT_SOURCE_DIR}/setupReflHeaderID.cmake"
        COMMENT "Generating new reflection header ID"
)

# Dummy target so we can set up a dependency
add_custom_target(
        gen-ReflectionHeaderID_h
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/generated_files/reflection_incs/ReflectionHeaderID.h"
)
set_target_properties(gen-ReflectionHeaderID_h PROPERTIES EXCLUDE_FROM_ALL true)



# We need this preprocess-only step to assemble the documentation
# for everything, otherwise  clang seems to throw it out when expanding #includes.
# ALSO, this assures we include only the stuff from our clang source code
# when running the clang tool -- I've been unable to get the clang tool
# to accept an -I with the proper includes; it keeps using my /usr/local/include/clang
# code, which will cause problems whenever that has different AST headers
# than the current clang.
# Bottom line preprocessing here avoids a few problems.
add_custom_target( preprocess_reflincludes
#        OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/generated_files/temp/ReflectionIncludesBig.hpp"
#        DEPENDS "${CLANG_SOURCE_DIR}/include/clang/AST/ReflectionIncludes.hpp" #TODO get this dpcy to work for all #included files in this file
        COMMAND "${CMAKE_CXX_COMPILER}" -E -C -P #preprocess, keep comments, no line markers
                "${CLANG_SOURCE_DIR}/include/clang/AST/ReflectionIncludes.hpp"
                --include-directory "${CLANG_SOURCE_DIR}/include/"
                --include-directory "${CLANG_BINARY_DIR}/include"
                -Wall -Wunknown-attributes -std=c++11 -x c++
                > "${CMAKE_CURRENT_SOURCE_DIR}/generated_files/temp/ReflectionIncludesBig.hpp"
        COMMENT "Preprocessing clang/AST/ReflectionIncludes.h into generated_files/temp/ReflectionIncludesBig.hpp (hack to make all doxygen comments available to Clang tool -- otherwise preprocessor removes them for any #includes) [DWR]"
)
#add_custom_target(
#        preprocess_reflincludes
#        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/generated_files/temp/ReflectionIncludesBig.hpp"
#)
set_target_properties(preprocess_reflincludes PROPERTIES EXCLUDE_FROM_ALL true)

add_compile_definitions(GENERATED_FILES_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}/generated_files/\")

add_clang_executable(reflection-src-generator
        EXCLUDE_FROM_ALL
        GenReflectionSrc.cpp
        )

add_dependencies(reflection-src-generator preprocess_reflincludes)
add_dependencies(reflection-src-generator gen-ReflectionHeaderID_h)
target_link_libraries(reflection-src-generator
        PRIVATE
        clangAST
        clangBasic
        clangFrontend
        clangTooling
        )

# Copy the "blank" reflection_incs over.
# Build this target whenever you cause an error while generating
# reflection sources that prevent clang from rebuilding, which
# we need to build the very tool that generates the reflection
# sources.  I.e. without somethign like this you can become
# stuck in a catch-22: needing a broken compiler to compile
# its own repairs.  Run this once to fix such issues.
add_custom_target(clean_reflection_incs)
set_target_properties(clean_reflection_incs PROPERTIES EXCLUDE_FROM_ALL true)
add_custom_command(TARGET clean_reflection_incs
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
                "${CMAKE_CURRENT_SOURCE_DIR}/blank_reflection_incs/"
                "${CLANG_SOURCE_DIR}/lib/Sema/reflection_incs/"
        COMMENT ".......Clearing old reflection sources (by copying blank_reflection_incs contents to Sema/reflection_incs)."
        )

# Actually for now let's just always clean first, to minimize issues:
add_dependencies(reflection-src-generator clean_reflection_incs)


add_custom_target(generate-reflection-srcs)
set_target_properties(generate-reflection-srcs PROPERTIES EXCLUDE_FROM_ALL true)

# Build reflection-src-generator
add_dependencies(generate-reflection-srcs reflection-src-generator)

# Run reflection-src-generator
add_custom_command(TARGET generate-reflection-srcs
        POST_BUILD
        COMMAND "reflection-src-generator"
        COMMENT ".......Generating reflection source files -- this may take a couple minutes..."
        )
# Copy the generated files to Sema/reflection_incs.
add_custom_command(TARGET generate-reflection-srcs
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
                "${CMAKE_CURRENT_SOURCE_DIR}/generated_files/reflection_incs/"
                "${CLANG_SOURCE_DIR}/lib/Sema/reflection_incs/"
        COMMENT ".......Copying generated reflection source files to lib/Sema/reflection_incs"
        )


# Finally, rebuild clang and copy the client_reflection_impl.hpp file to the same directory
# as the binary it just compiled.  (The user has to manually fish it out of there and
# put it in the desired include directory; this is in part to avoid conflicts between
# conflicting client_reflection_impl.hpp files from different builds.)
add_clang_executable(clang-wreflection GenReflectionSrc.cpp)
set_target_properties(clang-wreflection PROPERTIES EXCLUDE_FROM_ALL true)
target_link_libraries(clang-wreflection
        PRIVATE
        clangAST
        clangBasic
        clangFrontend
        clangTooling
        )

add_dependencies(clang-wreflection generate-reflection-srcs)

add_custom_command(TARGET clang-wreflection
        POST_BUILD
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
        COMMAND make clang
        COMMENT ".......Rebuilding clang with newly generated reflection sources")

add_custom_command(TARGET clang-wreflection
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        "${CMAKE_CURRENT_SOURCE_DIR}/generated_files/client_reflection_impl.hpp"
        "${CMAKE_BINARY_DIR}/include/."
        COMMENT ".......Copying newly generated client_reflection_impl.hpp to ${CMAKE_BINARY_DIR}/include")

