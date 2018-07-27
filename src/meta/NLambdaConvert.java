import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class NLambdaConvert {

    public static final List<String> DIGITS = Arrays.asList("x", "0", "1");
    public static final int SIZE = 8;

    public static void main(String[] args) {
        for (int i = 2; i < SIZE; i++) {
            convert(i);
        }
    }

    private static void convert(int i) {
        for (List<Integer> perm : permutations(i)) {
            if (perm.contains(1) || perm.contains(2)) {
                function(perm);
            }
        }
    }

    private static List<List<Integer>> permutations(int i) {
        if (i == 0) {
            return new ArrayList<>();
        }
        List<List<Integer>> result = new ArrayList<>();
        List<List<Integer>> perms = permutations(i - 1);
        for (int k = 0; k < 3; k++) {
            if (perms.isEmpty()) {
                result.add(Collections.singletonList(k));
            } else {
                for (List<Integer> perm : perms) {
                    List<Integer> perm1 = new ArrayList<>();
                    perm1.add(k);
                    perm1.addAll(perm);
                    result.add(perm1);
                }
            }
        }
        return result;
    }

    private static void function(List<Integer> perm) {
        String name = "convert" + perm.stream().map(DIGITS::get).collect(Collectors.joining());
        StringBuilder result = type(name, perm);
        impl(name, perm, result);
        System.out.println(result);
    }

    private static void impl(String name, List<Integer> perm, StringBuilder result) {
        result.append(name);
        if (perm.size() > 1) {
            result.append(" f");
        }
        if (perm.size() < 3) {
            result.append(" x");
        } else {
            IntStream.range(1, perm.size()).forEach(i -> result.append(" x").append(i));
        }
        result.append(" =");
        if (perm.get(perm.size()-1) == 1) {
            result.append(" value $");
        } else if (perm.get(perm.size()-1) == 2) {
            result.append(" noMeta $");
        }


        if (perm.size() > 1) {
            result.append(" f");
        }
        IntStream.range(0, perm.size()-1).forEach(i -> {
            switch (perm.get(i)) {
                case 0:
                    result.append(" x");
                    if (perm.size() > 2) {
                        result.append(i+1);
                    }
                    break;
                case 1:
                    result.append(" (noMeta x");
                    if (perm.size() > 2) {
                        result.append(i+1);
                    }
                    result.append(")");
                    break;
                case 2:
                    result.append(" (value x");
                    if (perm.size() > 2) {
                        result.append(i+1);
                    }
                    result.append(")");
                    break;
            }
        });
        result.append("\n");
    }

    private static StringBuilder type(String name, List<Integer> perm) {
        StringBuilder result = new StringBuilder(name);
        result.append(" :: ");
        if (perm.size() > 1) {
            result.append("(");
        }
        result.append(IntStream.range(0, perm.size())
            .mapToObj(i -> {
                String type = "";
                if (perm.get(i) == 1) {
                    type += "WithMeta ";
                }
                type += (char)('a'+i);
                return type;
            }).collect(Collectors.joining(" -> ")));
        if (perm.size() > 1) {
            result.append(")");
        }
        result.append(" -> ");
        result.append(IntStream.range(0, perm.size())
                .mapToObj(i -> {
                    String type = "";
                    if (perm.get(i) == 2) {
                        type += "WithMeta ";
                    }
                    type += (char)('a'+i);
                    return type;
                }).collect(Collectors.joining(" -> ")));
        result.append("\n");
        return result;
    }
}
