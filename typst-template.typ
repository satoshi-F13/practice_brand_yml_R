  #let company_info = {
    text(
      size: 9pt,
      [
        SATOM OÜ\
        Tule 29, Saue vald, 76202, Estonia
      ]
    )
  }
  
  #let contact_info = {
    text(
      size: 9pt,
      [
        KMKR: EE16275555\
        URL: www.satom.eu
      ]
    )
  }
  
#let stm-report(
  title: "title",
  subtitle: "subtitle",
  font: "Alata",
    body,
  ) = {
  // Set the document metadata
  set document(title: title)
  
  // Apply font styles globally
  set text(font: "NOTO Sans", size: 12pt)
  
  // Configure paragraph settings
  set par(justify: true, leading: 0.65em)
  


    set page(
      "us-letter",
      margin: (left: 1in, right: 1in, top: 1.95in, bottom: 1.5in),
 
      header: 
      
  // Define the header for the first page
             context { 
    if (counter(page).get().at(0) == 1) {
      // First page header with grid layout
      grid(
        columns: (70%, 30%),
        rows: auto,
        // Title as text
          align(bottom, text(font: "Alata", size: 30pt, fill: rgb("#2E2B70"), weight: "bold", title)),
        // Insert image
        box(width: 50pt,height: 50pt), // Empty box-space
        grid.cell(colspan: 1,align: horizon, text(font: "Alata", size: 18pt, fill:rgb("#2E2B70"), weight: "medium",subtitle)),
        // Insert image
        box(width: 50pt,height: 50pt) // Empty box-space
      )
        //Insert horizontal line
        place(
            dx: -1in,  // Offset by left margin
            dy: -0.3em, // Adjust vertical position as needed
            rect(
              width: 140%, // Add margins to both sides
              height: 0.1in,
              fill: rgb("#375A9D")
            )
          )
    } else if (counter(page).get().at(0) == counter(page).final().at(0)) {
      // Last page header
      grid(
        columns: (30%, 40%, 30%),
         // subtitle using the parameterized variable
          place(dy: -0.5in, text(font: "Alata", size: 10pt, fill: rgb("#2E2B70"), weight: "bold", subtitle)),
          // Title
          place(dy: -0.5in, center, text(font: "Alata", size: 15pt, fill: rgb("#2E2B70"), weight: "light", title)),
          )
      place(
            dx: -1in,  // Offset by left margin
            dy: -0.15in, // Adjust vertical position as needed
            rect(
              width: 140%, // Add margins to both sides
              height: 0.1in,
              fill: rgb("#A47D59")
            )
          )
    } else {
      
      // Regular header for other pages
     grid(
        columns: (30%, 40%, 30%),
         // subtitle using the parameterized variable
          place(dy: -0.5in, text(font: "Alata", size: 10pt, fill: rgb("#2E2B70"), weight: "bold", subtitle)),
          // Title
          place(dy: -0.5in, center, text(font: "Alata", size: 15pt, fill: rgb("#2E2B70"), weight: "light", title)),

      )
      place(
            dx: -1in,  // Offset by left margin
            dy: -0.15in, // Adjust vertical position as needed
            rect(
              width: 140%, // Add margins to both sides
              height: 0.1in,
              fill: rgb("#CB6874")
            )
          )
    }
  },
      footer: {
            align(top, line(length: 100%, stroke: (thickness: 1pt, paint: rgb("#A6AEBF"))))
          context { if(counter(page).get().at(0)== 1) {
          grid(
            columns: (30%, 40%, 30%),
            gutter: 10pt,
            align(left, company_info),
            place(dy: -1em, center, image("logos/icon/satom-icon-color.png", height: 40pt)),
            align(right , contact_info)
          )
  } else {
    // Define the header for all other pages
              grid(
            columns: (30%, 40%, 30%),
            gutter: 10pt,
              align(left , 
                  context [
                            #set align(left)
                            #set text(10pt)
                            #counter(page).display(
                                                  "1 / 1",
                                                  both: true,
                                                   )
                          ]),
            place(dy: -1em,center , image("logos/icon/satom-icon-color.png", height: 40pt)),
          )
          }
        }
      }
    )


  // Show raw code blocks in IBM Plex Mono
  show raw: set text(font: "IBM Plex Mono")
  
  // Return the body wrapped in NOTO Sans font
  set text(font: "NOTO Sans")
  body
  
  }