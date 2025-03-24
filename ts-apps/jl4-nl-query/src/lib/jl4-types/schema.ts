// import * as z from 'zod'

// const PrimitiveTypeSchema = z.object({
//   $type: z.literal('PrimitiveType'),
//   name: z.enum(['STRING', 'NUMBER', 'BOOLEAN']),
// })

// /** simplifying for now */
// const TypeSchema = PrimitiveTypeSchema

// const ParamSchema = z.object({
//   name: z.string(),
//   type: TypeSchema,
// })

// const FieldSchema = ParamSchema

// const FunctionDeclSchema = z.object({
//   kind: z.literal('FunctionDeclaration'),
//   name: z.string(),
//   params: z.array(ParamSchema),
//   returnType: TypeSchema,
// })

// const RecordDeclSchema = z.object({
//   kind: z.literal('RecordDeclaration'),
//   name: z.string(),
//   fields: z.array(FieldSchema),
// })

// const ConDeclSchema = z.object({
//   name: z.string(),
//   args: z.array(ParamSchema),
// })

// const EnumDeclSchema = z.object({
//   kind: z.literal('EnumDeclaration'),
//   name: z.string(),
//   constructors: z.array(ConDeclSchema),
// })

// const TypeDeclSchema = z.discriminatedUnion('kind', [
//   FunctionDeclSchema,
//   RecordDeclSchema,
//   EnumDeclSchema,
// ])

// export type PrimitiveType = z.infer<typeof PrimitiveTypeSchema>
// export type Type = PrimitiveType

// export type Param = z.infer<typeof ParamSchema>
// export type Field = z.infer<typeof FieldSchema>
// export type ConDecl = z.infer<typeof ConDeclSchema>

// export type FunctionDecl = z.infer<typeof FunctionDeclSchema>
// export type RecordDecl = z.infer<typeof RecordDeclSchema>
// export type EnumDecl = z.infer<typeof EnumDeclSchema>
// export type TypeDecl = z.infer<typeof TypeDeclSchema>
