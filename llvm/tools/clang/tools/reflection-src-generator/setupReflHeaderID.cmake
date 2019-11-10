# Randomly generated reflection header id, shared between the client
# and the compiler -- the compiler will make sure they match before
# reflecting anything to avoid errors.
# Note we don't include zeros, just so that the first digit isn't
# a zero, which would cause erroneous interpretation as an octal constant.
string(RANDOM LENGTH 4 ALPHABET 123456789 CMAKE_GEN_REFLHEADERID)

configure_file(
        "${THE_CURSOURCE_DIR}/ReflectionHeaderID.h.in"
        "${THE_CURSOURCE_DIR}/generated_files/reflection_incs/ReflectionHeaderID.h")