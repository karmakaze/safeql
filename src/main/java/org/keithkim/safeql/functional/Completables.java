package org.keithkim.safeql.functional;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

public class Completables {
    public static <T> CompletionStage<Void> consume(CompletionStage<T> a,
                                                    Consumer<? super T> consumer) {
        return a.thenAccept(consumer);
    }

    public static <T, U> CompletionStage<Void> consume(CompletionStage<T> a,
                                                       CompletionStage<? extends U> b,
                                                       BiConsumer<? super T, ? super U> consumer) {
        return a.thenAcceptBoth(b, consumer);
    }

    public static <T> CompletionStage<Void> consumeAny(CompletionStage<T> a,
                                                       CompletionStage<? extends T> b,
                                                       Consumer<? super T> consumer) {
        return a.acceptEither(b, consumer);
    }

    public static <T> CompletionStage<Void> consumeAny(List<CompletableFuture<? extends T>> futures,
                                                       final Consumer<? super T> consumer) {
        final CompletableFuture<Void> completable = new CompletableFuture<>();
        for (CompletableFuture<? extends T> future : futures) {
            future.whenComplete((t, e) -> {
                if (e == null) {
                    if (completable.complete(null)) {
                        consumer.accept(t);
                    }
                } else {
                    completable.completeExceptionally(e);
                }
            });
        }
        return completable;
    }

    public static <T> CompletionStage<Void> consumeAnyAsync(List<CompletableFuture<? extends T>> futures,
                                                            final Consumer<? super T> consumer) {
        final CompletableFuture<Void> completable = new CompletableFuture<>();
        for (CompletableFuture<? extends T> future : futures) {
            future.whenCompleteAsync((t, e) -> {
                if (e == null) {
                    if (completable.complete(null)) {
                        consumer.accept(t);
                    }
                } else {
                    completable.completeExceptionally(e);
                }
            });
        }
        return completable;
    }

    public static <T, U> CompletionStage<U> chain(CompletionStage<T> a,
                                                  Function<? super T, ? extends U> fn) {
        return a.thenApply(fn);
    }

    public static <T, U, V> CompletionStage<V> chain(CompletionStage<T> a,
                                                     Function<? super T, ? extends U> fn1,
                                                     Function<? super U, ? extends V> fn2) {
        return a.thenApply(fn1).thenApply(fn2);
    }

    public static <T, U, V, W> CompletionStage<W> chain(CompletionStage<T> a,
                                                        Function<? super T, ? extends U> fn1,
                                                        Function<? super U, ? extends V> fn2,
                                                        Function<? super V, ? extends W> fn3) {
        return a.thenApply(fn1).thenApply(fn2).thenApply(fn3);
    }

    public static <T, U, V, W, X> CompletionStage<X> chain(CompletionStage<T> a,
                                                           Function<? super T, ? extends U> fn1,
                                                           Function<? super U, ? extends V> fn2,
                                                           Function<? super V, ? extends W> fn3,
                                                           Function<? super W, ? extends X> fn4) {
        return a.thenApply(fn1).thenApply(fn2).thenApply(fn3).thenApply(fn4);
    }

    public static <T, U, V> CompletionStage<V> combine(CompletionStage<T> a,
                                                       CompletionStage<? extends U> b,
                                                       BiFunction<T, U, V> fn) {
        return a.thenCombine(b, fn);
    }

    public static <T, U, V, W> CompletionStage<W> combineAsync(CompletionStage<T> a,
                                                               CompletionStage<? extends U> b,
                                                               CompletionStage<? extends V> c,
                                                               TriFunction<? super T, ? super U, ? super V, W> fn) {
        return a.thenCompose(t ->
            b.thenCombineAsync(c, (u, v) -> fn.apply(t, u, v))
        );
    }

    public static <T, U, V, W, X> CompletionStage<X> combineAsync(CompletionStage<T> a,
                                                                  CompletionStage<? extends U> b,
                                                                  CompletionStage<? extends V> c,
                                                                  CompletionStage<? extends W> d,
                                                                  QuadFunction<? super T, ? super U, ? super V, ? super W, X> fn) {
        return a.thenCompose(t ->
                b.thenCompose(u ->
                        c.thenCombineAsync(d, (v, w) -> fn.apply(t, u, v, w))
                )
        );
    }

    public interface TriFunction<T, U, V, R> {
        R apply(T t, U u, V v);
    }

    public interface QuadFunction<T, U, V, W, R> {
        R apply(T t, U u, V v, W w);
    }
}
