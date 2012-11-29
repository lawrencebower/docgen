package org.lawrencebower.docgen.web_logic.business.product_injection.component;

import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.web_logic.business.product_injection.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public class TableComponentProductInjector implements ComponentProductInjector<TableComponent> {

    public void injectProducts(TableComponent tableComponent, List<ProductView> products) {

        clearExistingProducts(tableComponent);

        for (ProductView product : products) {
            TableRow row = getProductRow(tableComponent, product);
            tableComponent.addRow(row);
        }
    }

    private void clearExistingProducts(TableComponent docComponent) {
        docComponent.clearRows();
    }

    private TableRow getProductRow(TableComponent docComponent, ProductView product) {

        TableRow row = new TableRow();

        TableHeaderRow headerRow = docComponent.getHeaderRow();
        List<TableCell> headerCells = headerRow.getCells();
        for (TableCell headerCell : headerCells) {
            TableCell cell = getCellForColumn(headerCell, product);
            row.addCell(cell);
        }

        return row;
    }

    private TableCell getCellForColumn(TableCell headerCell, ProductView product) {

        TableCell newCell;

        String columnName = headerCell.getName();
        if(ProductInjectionField.containsName(columnName)){
            ProductInjectionField productField = ProductInjectionField.getByFieldName(columnName);
            newCell = makeCellForField(productField, product);
        }else{
            newCell = new TableCell("");//empty cell
        }

        return newCell;
    }

    private TableCell makeCellForField(ProductInjectionField productField, ProductView product) {
        String value = ProductInjectionField.getProductFieldByType(productField, product);
        return new TableCell(value);
    }
}
