# `wapco`

Source code to reproduce results from our papers on verification of approximate hardware designs:

- "*Verification of Approximate Hardware Designs*" submitted to WAPCO'23, co-located with HiPEAC'23. Run the following command to generate results:
  ```
  sbt "testOnly wapco.StandaloneWapcoSpec"
  ```
- "*Verification of Approximate Hardware Designs with ChiselVerify*" submitted to the special session on digital design and verification with Chisel at NorCAS'23. Run the following command to generate results
  ```
  sbt "testOnly norcas.StandaloneNorCASSpec norcas.CMVMNorCASSpec"
  ```

The code has multiple dependencies that must be pulled to compile and run it.
