package org.lawrencebower.docgen.web_logic.business.component_calculation.table;

import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;

import java.util.List;

public class TableComponentCalculation extends ComponentCalculation {

    private String targetColumn;

    public TableComponentCalculation(Operator operator,
                                     String targetColumn,
                                     List<String> fields) {
        super(operator, fields);
        this.targetColumn = targetColumn;
    }

    public TableComponentCalculation(Operator operator,
                                     String targetColumn,
                                     String... fields) {
        super(operator, fields);
        this.targetColumn = targetColumn;
    }

    public String getTargetColumn() {
        return targetColumn;
    }
}
