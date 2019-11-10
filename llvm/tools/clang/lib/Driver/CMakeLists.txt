set(LLVM_LINK_COMPONENTS
  BinaryFormat
  Option
  Support
  )

if(WIN32)
  # MSVCToolChain.cpp uses version.dll.
  set(system_libs version)
endif()

# DWR ADDN: add headers etc. so they are visible when imported into QT Creator
file(GLOB CLANG_DRIVER_INCLUDES
  "${CLANG_SOURCE_DIR}/include/clang/Driver/*.hpp"
  "${CLANG_SOURCE_DIR}/include/clang/Driver/*.h"
  "${CLANG_SOURCE_DIR}/include/clang/Driver/*.def"
  "${CLANG_SOURCE_DIR}/include/clang/Driver/*.td"
  )

add_clang_library(clangDriver
  ${CLANG_DRIVER_INCLUDES} #DWR ADDN

  Action.cpp
  Compilation.cpp
  Distro.cpp
  Driver.cpp
  DriverOptions.cpp
  Job.cpp
  Multilib.cpp
  Phases.cpp
  SanitizerArgs.cpp
  Tool.cpp
  ToolChain.cpp
  ToolChains/Arch/AArch64.cpp
  ToolChains/Arch/ARM.cpp
  ToolChains/Arch/Mips.cpp
  ToolChains/Arch/PPC.cpp
  ToolChains/Arch/RISCV.cpp
  ToolChains/Arch/Sparc.cpp
  ToolChains/Arch/SystemZ.cpp
  ToolChains/Arch/X86.cpp
  ToolChains/Ananas.cpp
  ToolChains/AMDGPU.cpp
  ToolChains/AVR.cpp
  ToolChains/BareMetal.cpp
  ToolChains/Clang.cpp
  ToolChains/CloudABI.cpp
  ToolChains/CommonArgs.cpp
  ToolChains/Contiki.cpp
  ToolChains/CrossWindows.cpp
  ToolChains/Cuda.cpp
  ToolChains/Darwin.cpp
  ToolChains/DragonFly.cpp
  ToolChains/FreeBSD.cpp
  ToolChains/Fuchsia.cpp
  ToolChains/Gnu.cpp
  ToolChains/Haiku.cpp
  ToolChains/HIP.cpp
  ToolChains/Hexagon.cpp
  ToolChains/Linux.cpp
  ToolChains/MipsLinux.cpp
  ToolChains/MinGW.cpp
  ToolChains/Minix.cpp
  ToolChains/MSVC.cpp
  ToolChains/Myriad.cpp
  ToolChains/NaCl.cpp
  ToolChains/NetBSD.cpp
  ToolChains/OpenBSD.cpp
  ToolChains/PS4CPU.cpp
  ToolChains/ToolchainsRISCV.cpp #DWR MOD to avoid warning about same member name
  ToolChains/Solaris.cpp
  ToolChains/TCE.cpp
  ToolChains/WebAssembly.cpp
  ToolChains/XCore.cpp
  Types.cpp
  XRayArgs.cpp

  DEPENDS
  ClangDriverOptions

  LINK_LIBS
  clangBasic
  ${system_libs}
  )
