package org.lawrencebower.docgen.web_logic.business.component_calculation;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ComponentCalculation {

    private Operator operator;
    private List<String> fields;
    private boolean isRun;

    public ComponentCalculation(Operator operator, List<String> fields) {
        this.operator = operator;
        this.fields = Collections.unmodifiableList(fields);
    }

    public ComponentCalculation(Operator operator, String... fields) {
        this.operator = operator;
        this.fields = Arrays.asList(fields);
    }

    public Operator getOperator() {
        return operator;
    }

    public List<String> getOperands() {
        return fields;
    }

    public boolean isNotRun() {
        return !isRun;
    }

    public boolean isRun() {
        return isRun;
    }
}
