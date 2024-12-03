export async function fetchJson<T>(url: string): Promise<T> {
  const response = await fetch(url)
  if (!response.ok) throw new Error('failed to fetch json')
  return response.json() as Promise<T>
}

export async function fetchUpdates<T>(
  url: string,
  previousData: T
): Promise<T | null> {
  const response = await fetch(url)
  if (!response.ok) throw new Error('failed to get updated json')

  const newData = (await response.json()) as T

  if (JSON.stringify(newData) !== JSON.stringify(previousData)) {
    return newData
  }

  return null
}

export async function sendToLadder<T>(url: string, data: unknown): Promise<T> {
  const response = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(data),
  })
  if (!response.ok) throw new Error('failed to send to ladderdiagram')
  return response.json() as Promise<T>
}
