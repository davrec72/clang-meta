//DWR ADDN:
//
// Created by David Rector on 2019-02-24.
//

#include "clang/Lex/MetaparseDebug.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"

using namespace clang;

//Static variable initialization:
#ifndef NDEBUG
unsigned Parser::DEBUGnumParsersConstructed = 0;
#endif
#ifndef NMETAPARSEDEBUG
unsigned ParserBrickWallRAII::DEBUGrecursiondepth = 0;
unsigned METAPARSE_DEBUG_curindent_RAII::indent = 0;
#endif
//END