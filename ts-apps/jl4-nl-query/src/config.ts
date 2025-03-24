import { config } from 'dotenv'

import { Logger } from 'tslog'

/*****************
  Load .env
******************/

const envPath = process.env.envPath || './.env.local'

config({
  path: envPath,
})

/*****************
   Init logger
******************/

const logger = new Logger({
  name: 'nlq-logger',
  // minLevel: 1,
  prettyLogTemplate: '{{logLevelName}}  ',
})

export type AppLogger = typeof logger

// TODO: Remove this before pushing to prod
logger.debug(
  process.env.OPENAI_API_KEY,
  process.env.REMOTE_DECISION_SERVICE_URL
)

/*****************
   App Config
******************/

export class AppConfig {
  #logger: AppLogger = logger
  #jl4DecisionServiceUrl: string
  #openAiApiKey: string

  constructor(env: typeof process.env, logger: AppLogger) {
    this.#jl4DecisionServiceUrl = env.REMOTE_DECISION_SERVICE_URL as string
    this.#openAiApiKey = env.OPENAI_API_KEY as string

    this.#logger = logger
  }

  getDecisionServiceUrl() {
    return this.#jl4DecisionServiceUrl
  }

  getLlmApiKey() {
    return this.#openAiApiKey
  }

  getLogger() {
    return this.#logger
  }
}

export const appConfig = new AppConfig(process.env, logger)
