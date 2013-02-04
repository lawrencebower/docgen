package org.lawrencebower.docgen.core.document.component.table;

import java.util.List;

public interface TableRow<T extends TableCell> {

    int getColumnCount();

    void addCell(T cell);

    List<T> getCells();

    T getCell(int colNum);

    int getCellCount();
}
