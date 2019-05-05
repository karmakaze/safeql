package org.keithkim.safeql;

import java.time.Instant;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;

public class PlayCompletable {
    public static void log(String line) {
        String thread = Thread.currentThread().getName().replace("ForkJoinPool.commonPool-", "");
        java.lang.System.out.println(Instant.ofEpochMilli(java.lang.System.currentTimeMillis()) + " [" + thread + "] " + line);
    }

    public static void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

// return                       condition await  computation
// ------  -------------------- --------- -----  -----------
//         CompletableFuture (static methods):
//
//    <T>  completedFuture
//    <T>  completedStage
//    <T>  failedFuture
//    <T>  failedStage
// <Void>  runAsync                              Runnable
//    <T>  supplyAsync                           Supplier<T>
//
//         CompletionStage<T>:
//
// <Void>  thenRun/Async        normal    1 / 1  Runnable
// <Void>  thenAccept/Async     normal    1 / 1  Consumer<? super T>
// <Void>  runAfterEither/Async normal*   1 / 2  Runnable
// <Void>  acceptEither/Async   normal*   1 / 2  Consumer<? super T>
// <Void>  runAfterBoth/Async   normal    2 / 2  Runnable
// <Void>  thenAcceptBoth/Async normal    2 / 2  BiConsumer<? super T, ? super U>
//
//    <T>  whenComplete/Async   always    1 / 1  BiConsumer<? super T, ? super Throwable>
//    <T>  exceptionally        error     1 / 1  Function<Throwable, ? extends T>
//    <T>  toCompletableFuture  always    1 / 1
//
//    <U>  handle/Async         always    1 / 1  BiFunction<? super T, Throwable>, ? extends U>
//    <U>  thenApply/Async      normal    1 / 1  Function<? super T, ? extends U>
//    <U>  thenCompose/Async    normal    1 / 1  Function<? super T, ? extends CompletionStage<U>>
//    <U>  applyToEither/Async  normal*   1 / 2  Function<? super T, U>
//
//    <V>  thenCombine/Async    normal    2 / 2  BiFunction<? super T, ? super U, ? extends V>
//
// * 'Either' forms may proceed normally or exceptionally when exactly one throws
//   'Async' forms are scheduled to run in an executor pool (default or specified)
//   non-'Async' forms run on the thread that completes the previous stage

    public <T, U> U apply(T value, Function<T, U> fn) {
        return fn.apply(value);
    }

    public <T, U> U apply(CompletableFuture<T> future, Function<T, U> fn) {
        return fn.apply(future.join());
    }

    public <T, U> CompletableFuture<U> compose(CompletableFuture<T> future, Function<T, CompletableFuture<U>> fn) {
        return future.thenCompose(fn);
    }

    public static void main(String[] args) {
        CompletableFuture<String> completable1 = CompletableFuture.supplyAsync(() -> {
            sleep(1_000);
            log("returning from 1");
            return "Hello, there.";
        });

        CompletableFuture<String> completable2 = completable1.handleAsync((result, error) -> {
            sleep(1_000);
            log("returning from 2");
            return "The second";
        });

        CompletableFuture<String> completable3 = completable1.thenApply(result -> {
            sleep(1_000);
            log("returning from 3");
            return "The third: " + result;
        });

        CompletableFuture<String> completable4 = completable1.thenApplyAsync(result -> {
            sleep(1_000);
            log("returning from 4");
            return "The fourth: " + result;
        });

        try {
            log("Started...");
            log("Got 1: " + completable1.get());
            log("Got 2: " + completable2.get());
            log("Got 3: " + completable3.get());
            log("Got 4: " + completable4.get());
            sleep(3_000L);
        } catch (ExecutionException |InterruptedException e) {
            e.printStackTrace();
        }
    }
}
