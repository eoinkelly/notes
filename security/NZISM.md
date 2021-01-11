# NZISM


Which bits can I ignore because we don't run data centers?

## Overview

23 sections

Sections most applicable to creating web apps and their infra:

4 - know the C&A process agencies will need to follow
5 - know the docs agencies are expected to create/maintain
14


> Agencies engaging industry for the provision of off-site information technology services and functions MUST accredit the systems used by the contractor to at least the same minimum standard as the agency&#x2019;s systems. This may be achieved through a third party review report utilising the ISAE 3402 Assurance Reports on Controls at a Third Party Service Organisation.
might be relevant for us

## Section 1

NZISM Contains

1. Processes
1. Controls

at levels

1. Essential Baseline processes and controls
1. Recommended Good practice processes and controls


Data classifications (not defined in NZISM but heavily referenced)

1. UNCLASSIFIED
1. IN-CONFIDENCE
1. SENSITIVE
1. RESTRICTED
1. CONFIDENTIAL
1. SECRET
1. TOP SECRET

_All Classifications_ is anything which has been marked with one of the above or is "Official Information"

NZISM defines a minimum set of controls for each level of data classification

Uses RFC 2119 definitons to say what compliance is required e.g. MUST, MUST NOT, SHOULD, SHOULD NOT

In opinion of NZISM, Anything with a MUST  is a risk which cannot be managed by an agency, thigns with a SHOULD are risks that can be managed

In cases where the manual cannot be fully complied with, the risk assessment should talk about _compensating controls_ and the acceptance of the residual risk
i.e. if you can't comply for some reason you need to provide a risk assessment of why


## Section 2

Nothing

## Section 3

The agency head is accountable for the secuiryt within their agency

PSR = Protective Security Requirements https://www.protectivesecurity.govt.nz/

## Section 4

Executives and Security Practitioners understand and enforce the use of the Certification and Accreditation (C&A) process and its role in information security governance and assurance.

They describe **the** C&A process not **a** C&A process

## Section 5

Describes the 9 security docs tha an agency will need to develop

1. Information Security Policy (MUST)
1. Systems Architecture (MUST)
1. Security Risk Management Plan (MUST)
1. System Security Plan (MUST)
1. Site Security Plan ???
1. Standard Operating Procedures (MUST)
1. Incident Response Plan (MUST)
1. Emergency Procedures (SHOULD)
1. Independent Assurance reports for externally provided services

## Section 6


> This section covers information on conducting reviews of any agency’s information security posture and security implementation.

## Section 7

> This section covers information relating to detecting information security incidents.  Detecting physical and personnel security incidents is out of scope of this section, unless there is an impact on information systems. Refer to Chapter 8 - Physical Security and Chapter 9 - Personnel Security.

## Section 8

> This section covers information on the physical security of facilities. Information on physical security controls for servers and network devices, network infrastructure and IT equipment can be found in the following sections of this chapter.

## Section 9

> This section covers information relating specifically to information security awareness and training.

## Section 10

> his section covers information relating to cable distribution systems used in facilities within New Zealand. When designing cable management systems, Section 10.5 - Cable Labelling and Registration and Section 10.6 - Cable Patching of this chapter also apply.

## Section 11

> This section covers information relating to the use of RF and infrared devices in secure areas. Information on the use of RF devices outside secure areas can be found in Chapter 21 - Working Off-Site.

## Section 12

All about buying security products.

> This section covers information on the selection and acquisition of any product that provide security functionality for the protection of information. It DOES NOT provide information on the selection or acquisition of products that do not provide security functionality or physical security products.

## Section 13

> This section discusses the retirement and safe decommissioning of systems. Specific requirements on media handling, usage, sanitisation, destruction and disposal are discussed later in this chapter. System decommissioning is the retirement or termination of a system and its operations. System decommissioning does NOT deal with the theft or loss of equipment.

## Section 14

> This section covers information on the hardening of software used on workstations and servers on systems within agency control.


14.1 about hardening workstations and servers - definitely applies

covers not installing stuff you don't need etc.
i think this is the one that covers provisioning?

14.1.8.C.01.Control: System Classification(s): All Classifications; Compliance: SHOULD [CID:1149]
    Agencies SHOULD develop a hardened SOE for workstations and servers, c

14.2 app whitelisting (only certain apps shoudl be allowed to run)


14.3 is about web browser security


14.4 Software app development
* SHOULD have dev -> testing -> prod envs
* SHOULD use secure coding practices (deny by default etc.)
* SHOULD do testing

14.5 Web Application Development

14.5.6 SHOULD review for security issues
14.5.7 SHOULD Segregate application components
14.5.8 SHOULD Follow OWASP

## Section 15

> This section covers information on email policy and usage as it applies to content and protective markings.  I

Ignore

## Section 16

>
This section provides information on the identification and authentication of all system users.

This applies to us at both the cloud, server and app levels
covers password constraints

## Section 17

Encryption

Some of this applies e.g. TLS certs

## Section 18

> This section covers information relating to the selection, management and documentation of network infrastructure.

Some of this applies

## Section 19

Gateways and Firewalls
Some probably applies in cloud context

## Section 20

> This section covers the fundamental requirements of data transfers between systems and applies equally to data transfers using removal media and to data transfers via gateways.

Some of this applies

## Section 21

> This section covers information relating to the use of agency-owned mobile devices including, but not restricted to, mobile phones, smartphones, portable electronic devices, personal digital assistants, laptops, netbooks, tablet computers, and other portable Internet connected devices.

Ignore

## Section 22

22.1 Cloud Computing definitely applies
22.2 (Virtualisation) and 22.3 (VLANs) do not apply

## Section 23

Glossary etc.
can mostly ignore