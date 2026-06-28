---
name: effect-hono-api-design
description: Guide on building type-safe APIs using Effect v4 and Hono, including Hono request context DI, schema parsing, validation error wrapping, and atomic memory state management.
---

# Effect v4 + Hono API Design Guide

This skill documents architectural patterns for building type-safe web APIs with **Hono** and **Effect v4**. It covers runtime setups, request context injection, input validation using `Schema.Codec`, unified error mappings, and atomic memory mutations.

---

## 1. Setting Up Hono Context Service & Runtime

To allow endpoint programs to access Hono query parameters, headers, or request payloads directly, define a Hono Context service wrapper class.

### Core Setup (`server/core/effect.ts`)
```typescript
import { Context, Effect, ManagedRuntime, Exit, Cause } from "effect";
import type { Context as HonoContextShape, Handler } from "hono";
import { StorageService, StorageServiceLive } from "../services/storage";

// 1. Define the Hono Context Service class for Effect v4
export class HonoContext extends Context.Service<HonoContext, HonoContextShape>()("HonoContext") {}

// 2. Create the managed runtime with necessary global layers (e.g. StorageService)
export const runtime = ManagedRuntime.make(StorageServiceLive);

// 3. Helper to inspect Cause and find a failure reason
const getFailure = <E>(cause: Cause.Cause<E>): E | undefined => {
  for (const reason of cause.reasons) {
    if (Cause.isFailReason(reason)) return reason.error;
  }
  return undefined;
};
```

---

## 2. Constraining the Handler Environment (`defineHandler`)

To prevent handlers from requiring arbitrary unprovided services (which usually prompts unsafe `as any` casts), constrain the handler's environment `R` to extend only the services we have ready to provide (`HonoContext` and `StorageService`):

```typescript
/**
 * Creates a Hono Handler from an Effect program.
 * Injecting the HonoContext into the Effect program allows endpoints to read query params,
 * request bodies, headers, etc. directly from the Context.
 */
export const defineHandler = <A, E, R extends HonoContext | StorageService>(
  effect: Effect.Effect<A, E, R>
): Handler => {
  return async (c) => {
    // Provide the request context (c) to the HonoContext service tag
    const program = Effect.provideService(effect, HonoContext, c);

    // Run the program using our shared runtime (using runPromiseExit so we don't throw)
    const exit = await runtime.runPromiseExit(program);

    if (Exit.isSuccess(exit)) {
      const result = exit.value;
      if (result instanceof Response) return result;
      return c.json(result);
    } else {
      const cause = exit.cause;
      const failure = getFailure(cause);

      // Unified validation and domain error mapping to HTTP 400 Bad Request
      if (failure && typeof failure === "object" && "_tag" in failure && "errors" in failure) {
        const tag = (failure as any)._tag;
        if (tag === "InvalidBody" || tag === "InvalidParams" || tag === "InvalidQuery") {
          const readableError = tag.replace("Invalid", "Invalid ");
          return c.json({ error: readableError, details: (failure as any).errors }, 400);
        }
      }

      console.error("Effect execution failed with cause:", cause);
      return c.json({ error: "Internal Server Error", details: String(cause) }, 500);
    }
  };
};
```

---

## 3. Safe Schema Validations (`server/core/validation.ts`)

When decoding payloads, using `Schema.Schema` can default the third generic parameter (`DecodingServices`) to `unknown`, propagating `unknown` to your handler's environment requirement `R`. 
To avoid this, enforce `Schema.Codec<A, any, never>` to keep requirements clean:

```typescript
import { Schema, Effect } from "effect";
import { HonoContext } from "./effect";
import { InvalidBody, InvalidParams } from "./errors";

export const validateBody = <A>(
  schema: Schema.Codec<A, any, never>
): Effect.Effect<A, InvalidBody, HonoContext> =>
  Effect.gen(function* () {
    const c = yield* Effect.service(HonoContext);
    const json = yield* Effect.tryPromise({
      try: () => c.req.json(),
      catch: (e) => new InvalidBody({ errors: `Failed to parse JSON body: ${String(e)}` }),
    });
    return yield* Schema.decodeUnknownEffect(schema)(json).pipe(
      Effect.mapError((parseError) => new InvalidBody({ errors: String(parseError) }))
    );
  });

export const validateParams = <A>(
  schema: Schema.Codec<A, any, never>
): Effect.Effect<A, InvalidParams, HonoContext> =>
  Effect.gen(function* () {
    const c = yield* Effect.service(HonoContext);
    const params = c.req.param();
    return yield* Schema.decodeUnknownEffect(schema)(params).pipe(
      Effect.mapError((parseError) => new InvalidParams({ errors: String(parseError) }))
    );
  });
```

---

## 4. Atomic Concurrency-Safe Mutations (`Ref.modify`)

When updating in-memory state or shared references:
* **Anti-Pattern**: Running a sequence of `Ref.get(ref)` followed by checks, followed by `Ref.update(ref)`. This creates a **race condition** between concurrent fibers.
* **Remedy**: Perform updates atomically using `Ref.modify`. In Effect v4, `Either` is renamed to `Result`. We return a `Result` indicating success or failure from the modification transition, then flatMap:

```typescript
import { Context, Effect, Layer, Ref, Result } from "effect";

// ... Storage Service setup ...

const updateItem = (id: string, name: string, quantity: number, completed: boolean) =>
  Ref.modify(itemsRef, (items): [Result.Result<ShoppingItem, Error>, readonly ShoppingItem[]] => {
    const index = items.findIndex((item) => item.id === id);
    if (index === -1) {
      return [Result.fail(new Error(`Item with id ${id} not found`)), items];
    }
    const updatedItem: ShoppingItem = { id, name, quantity, completed };
    const nextItems = items.map((item) => (item.id === id ? updatedItem : item));
    return [Result.succeed(updatedItem), nextItems];
  }).pipe(Effect.flatMap(Effect.fromResult));
```

This ensures that the check and update occur as a single atomic step, guaranteeing consistency in concurrent execution models.
