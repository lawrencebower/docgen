package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;

import java.util.ArrayList;
import java.util.List;

public class TableComponent extends DocComponent {

    private List<TableRow> rows = new ArrayList<>();

    public TableComponent(String name) {
        super(name);
    }

    public TableComponent(String name, DocPosition position) {
        super(name, position);
    }

    public int getColumnCount() {
        if (rows.isEmpty()) {
            return 0;
        }
        return rows.get(0).getColumnCount();
    }

    public void addRow(TableRow row) {
        this.rows.add(row);
    }

    public void setRows(List<TableRow> rows) {
        this.rows = rows;
    }

    public List<TableRow> getRows() {
        return rows;
    }

    public List<TableCell> getAllCells(){
        List<TableCell> allCells = new ArrayList<>();
        for (TableRow row : rows) {
            allCells.addAll(row.getCells());
        }
        return allCells;
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.TABLE;
    }

}
