# README

## Example of how to use the api client

```typescript
import { APIClient } from '$lib/decision-api-client/api.js'
import { appConfig } from '../config.js'
const client = APIClient.make(appConfig)

client.getSimpleFunctionEndpoints().then((endpoints) => {
  console.log(endpoints)
})
```
