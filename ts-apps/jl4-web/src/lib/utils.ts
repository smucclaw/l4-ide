import { type ClassValue, clsx } from "clsx";
import { twMerge } from "tailwind-merge";

// https://gist.github.com/ca0v/73a31f57b397606c9813472f7493a940
export function debounce<T extends (...args: Parameters<T>) => ReturnType<T>>(
  callback: T,
  /** ms */
  delay: number,
) {
  let timer: ReturnType<typeof setTimeout>;
  return (...args: Parameters<T>) => {
    const p = new Promise<ReturnType<T> | Error>((resolve, reject) => {
      clearTimeout(timer);
      timer = setTimeout(() => {
        try {
          let output = callback(...args);
          resolve(output);
        } catch (err) {
          if (err instanceof Error) {
            reject(err);
          }
          reject(new Error(`An error has occurred:${err}`));
        }
      }, delay);
    });
    return p;
  };
}

/***********************
     Shadcn Utils
************************/

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}
