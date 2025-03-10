# Error Codes

| Code | Meaning |
|---|---|
| 0    | No error                          |
| 101  | Insufficient memory available     |
| 102  | No network data available         |
| 103  | Hydraulic solver not opened   |
| 104  | No hydraulics for water quality analysis |
| 105  | Water quality solver not opened |
| 106  | No results saved to report on |
| 107  | Hydraulics supplied from external file |
| 108  | Cannot use external file while hydraulics solver is open |
| 110  | Cannot solve network hydraulic equations |
| 120  | Cannot solve water quality transport equations |
|  ||
| 200  | One or more errors in an input file |
| 201  | Syntax error |
| 202  | Function call contains an illegal numeric value |
| 203  | Function call refers to an undefined node |
| 204  | Function call refers to an undefined link |
| 205  | Function call refers to an undefined time pattern |
| 206  | Function call refers to an undefined curve |
| 207  | Function call attempts to control a check valve pipe or a GPV valve |
| 208  | Function call contains illegal PDA pressure limits |
| 209  | Function call contains an illegal node property value |
| 211  | Function call contains an illegal link property value |
| 212  | Function call refers to an undefined Trace Node |
| 213  | Function call contains an invalid option value |
| 214  | Too many characters in a line of an input file |
| 215  | Function call contains a duplicate ID label |
| 216  | Function call refers to an undefined pump |
| 217  | Invalid pump energy data |
| 219  | Illegal valve connection to tank node |
| 220  | Illegal valve connection to another valve |
| 221  | Mis-placed clause in rule-based control |
| 222  | Link assigned same start and end nodes |
| 223  | Not enough nodes in network |
| 224  | No tanks or reservoirs in network |
| 225  | Invalid lower/upper levels for tank |
| 226  | No head curve or power rating for pump |
| 227  | Invalid head curve for pump |
| 230  | Nonincreasing x-values for curve |
| 233  | Network has unconnected node |
| 240  | Function call refers to nonexistent water quality source |
| 241  | Function call refers to nonexistent control |
| 250  | Function call contains invalid format (e.g. too long an ID name) |
| 251  | Function call contains invalid parameter code |
  252  | Function call contains an invalid ID name |
| 253  | Function call refers to nonexistent demand category |
| 254  | Function call refers to node with no coordinates |
| 257  | Function call refers to nonexistent rule |
| 258  | Function call refers to nonexistent rule clause |
| 259  | Function call attempts to delete a node that still has links connected to it |
| 260  | Function call attempts to delete node assigned as a Trace Node |
| 261  | Function call attempts to delete a node or link contained in a control |
| 262  | Function call attempts to modify network structure while a solver is open |
|  ||
| 301  | Identical file names used for different types of files |
| 302  | Cannot open input file |
| 303  | Cannot open report file |
| 304  | Cannot open output file |
| 305  | Cannot open hydraulics file |
| 306  | Hydraulics file does not match network data |
| 307  | Cannot read hydraulics file |
| 308  | Cannot save results to binary file |
| 309  | Cannot save results to report file |

