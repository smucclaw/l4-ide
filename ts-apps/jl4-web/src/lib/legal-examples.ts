// Load all .l4 files from the static examples directory at build time
const legalExampleFiles = import.meta.glob('/static/examples/*.l4', {
  as: 'raw',
  eager: true,
})

export interface LegalExample {
  id: string
  name: string
  content: string
}

// Transform the imported files into a structured format
export const legalExamples: LegalExample[] = Object.entries(
  legalExampleFiles
).map(([path, content]) => {
  const filename = path.split('/').pop()!.replace('.l4', '')

  return {
    id: filename,
    name: formatExampleName(filename),
    content: content as string,
  }
})

function formatExampleName(filename: string): string {
  return filename.replace(/[-_]/g, ' ').replace(/\b\w/g, (l) => l.toUpperCase())
}

// Fallback example (from the current hardcoded example in +page.svelte)
const fallbackExample: LegalExample = {
  id: 'british-citizen-fallback',
  name: 'British Citizen (Fallback)',
  content: `§ \`Assumptions\`

ASSUME Person IS A TYPE
ASSUME \`mother of\` IS A FUNCTION FROM Person TO Person
ASSUME \`father of\` IS A FUNCTION FROM Person TO Person

ASSUME \`is born in the United Kingdom after commencement\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is born in a qualifying territory after the appointed day\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is settled in the United Kingdom\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is settled in the qualifying territory in which the person is born\` IS A FUNCTION FROM Person TO BOOLEAN

§ \`The British Citizen Act\`

\`for father or mother of\` person property MEANS
      property OF \`father of\` person
   OR property OF \`mother of\` person

GIVEN p IS A Person
GIVETH A BOOLEAN
DECIDE \`is a British citizen (variant)\` IS
         \`is born in the United Kingdom after commencement\` p
      OR \`is born in a qualifying territory after the appointed day\` p
  AND -- when the person is born ...
         \`for father or mother of\` p \`is a British citizen (variant)\`
      OR \`for father or mother of\` p \`is settled in the United Kingdom\`
      OR \`for father or mother of\` p \`is settled in the qualifying territory in which the person is born\``,
}

// Export the first example as default for initial load, with fallback
export const defaultExample = legalExamples[0] || fallbackExample
