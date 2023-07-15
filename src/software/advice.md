# Career Advice for Software Engineers

- **Have a portfolio**
  - _Create your own publicly visible repositories on GitHub or the like_
  - _Contribute to open source projects_
  - _The type of projects are less important than quality_
    - Find software projects that connect to your interests and hobbies so as to
      motivate you to put in sufficient effort while still being fun / valuable
      to do in your own (unpaid) time
      - Games
      - Home automation / security
      - CNC / 3D printing
      - Music
      - Graphical arts
  - _Demonstrate proficiency in at least two widely-used programming languages_
    - Reasonable choices at the time of writing (changes every few years!):
      - Javascript
      - C / C++
      - Go
      - Python
      - Rust
      - Java
  - _Demonstrate good programming style and discipline in all publicly visible code_
    - Well documented
    - Well formatted
    - Use build automation that includes
      - Linting / static code analysis
      - Automated tests
      - Packaging
        - NPM compatible packages for Javascript
        - Executables and shared libraries for C / C++
        - Go modules
        - Maven-repository compatible JAR's for Java
        - Deployable units for front-end (varies widely by underlying tech stack)
        - You don't necessarily have to publish such packages to some "official"
          repository but demonstrate that you understand how such packaging works
        - That said, having some published packages along with your portfolio is
          a good thing
  - _For extra credit, have a blog or some such_
- **Be active in coding communities**
  - Answer questions on stackoverflow etc.
  - Create tutorials
  - Be active in the wiki / blog / etc. for open source projects you use or
    on which you collaborate
- **Do not over-specialize**
  - _Keep up to date on trends_
    - Programming languages commonly used in particular industries
    - Operating systems
    - Frameworks
    - Application domains
  - _...but there is a fine line between keeping up to date and chasing fads_
    - Depth of understanding in a few, but diverse, areas of concern using
      time-honored tools and techniques is more important than constantly
      dropping the latest buzzworthy jargon
  - _The "Full Stack Developer" is a mythological creature, still:_
    - Include a range of areas of concern in your portfolio
      - Front end
        - HTML / Javascript demonstrating understanding of the DOM
        - Frameworks like Vue/Vuetify, Angular, React
        - Mobile
      - Back end
        - Relational databases
        - Time series databases
        - Key / value storage
      - Real time
        - Home automation
        - CNC
      - AI
        - Query engineering
        - LLM development
        - Another area that changes rapidly and is always and forever full of
          the next over-hyped thing that is about to but hasn't yet quite
          changed every paradigm of human existence... so demonstrate awareness
          but don't over-invest
    - You do not need every one of all of the above in portfolio
    - ...But you do need a few from diverse areas of concern to demonstrate
      understanding of software architecture beyond a narrow niche

As an example, and to show that I practice what I preach:

- <https://github.com/parasaurolophus/home-automation>
  - Home-grown, in-home (no clouds!) home automation
    - This really does drive my lighting and window shades so I am motivated to
      keep it functional
  - Uses <https://nodered.org>, a community in which I participate
  - Extensive use of Node.js compatible Javascript
    - Inline Javascript in the Node-RED flows
    - Uses separately packaged and published NPM modules I also created
      (described below)
    - Accesses API's provided by Philips Hue and Hunter-Douglas devices
      using HTTP and _eventsource_ based services
  - Includes Vue / Vueitfy based front-end code
    - <https://github.com/parasaurolophus/home-automation/tree/main/dashboard>
  - Demonstrates
    - Separation of concerns
    - Modularity
    - Event-driven programming techniques
  - Note the fairly extensive _README.md_
    - <https://mermaid.js.org/> based architecture diagram
    - Detailed installation / configuration instructions
- <https://github.com/parasaurolophus/node-red-eventsource>
  - One of two library NPM packages I developed for use by my home automation system
    - The other is <https://github.com/parasaurolophus/node-red-dnssd>
  - Published to NPM and in the Node-RED flow library so that it can be used by
    the Node-RED community
  - Passes all quality checks performed by the Node-RED flow library
  - _README.md_ with screenshot, usage instructions
- <https://www.rader.us/music/digital.html>
  - And now for something completely different: Ruby source code as musical "scores"
- [./scheme-tail-recursion.html](scheme-tail-recursion.md)
  - One of many possible examples of why every programmer should learn
    [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
    (along with the assembly language of at least one microprocessor) if they
    want truly to understand how software really works &mdash; no pressure!
