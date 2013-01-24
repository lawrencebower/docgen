package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.DocComponent;

import java.util.List;

public interface TableComponent<
        T extends TableRow,
        T2 extends TableCell,
        T3 extends HeaderCell,
        T4 extends TableHeaderRow> extends DocComponent {

    int getColumnCount();

    T4 getHeaderRow();

    void addRow(T row);

    List<T> getRows();

    void clearRows();

    List<TableCell> getAllRenderableCells();

    void setWidthPercentage(float widthPercentage);

    float getWithPercentage();

    int[] getColumnWidths();

    void setTablePadding(int tablePadding);

    int getTablePadding();

    T getRow(int rowNum);
}
