package org.lawrencebower.docgen.web_logic.business.injection.product;

import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.view_table.*;
import org.lawrencebower.docgen.web_model.business_def.injection.TableComponentProductInjector;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class TableComponentProductInjectorImpl implements ComponentProductInjector<ViewTableComponent>, TableComponentProductInjector {

    @Autowired
    private ProductInjectionMapper productInjectionMapper;

    @Override
    public void injectProducts(ViewTableComponent tableComponent, List<ProductView> products) {

        clearExistingProducts(tableComponent);

        for (ProductView product : products) {
            ViewRow row = getProductRow(tableComponent, product);
            tableComponent.addRow(row);
        }
    }

    private void clearExistingProducts(TableComponent docComponent) {
        docComponent.clearRows();
    }

    private ViewRow getProductRow(ViewTableComponent tableComponent, ProductView product) {

        String productId = product.getProductId();

        ViewRow row = new ViewRow(productId);

        ViewHeaderRow headerRow = tableComponent.getHeaderRow();
        List<ViewHeaderCell> headerCells = headerRow.getCells();
        for (ViewHeaderCell headerCell : headerCells) {
            ViewCell cell = getCellForColumn(headerCell, product);
            row.addCell(cell);
        }

        return row;
    }

    private ViewCell getCellForColumn(ViewHeaderCell headerCell, ProductView product) {

        ViewCell newCell;

        String columnName = headerCell.getName();
        if(ProductInjectionField.containsName(columnName)){
            ProductInjectionField productField = ProductInjectionField.getByFieldName(columnName);
            newCell = makeCellForField(productField, product);
        }else{
            newCell = new ViewCell();//empty cell
        }

        return newCell;
    }

    private ViewCell makeCellForField(ProductInjectionField productField, ProductView product) {
        String value = productInjectionMapper.getProductFieldByType(productField, product);
        return new ViewCell(value);
    }
}
