package org.lawrencebower.docgen.web_model.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business.product_injection.TableComponentProductInjector;
import org.lawrencebower.docgen.web_model.business.table_component.TableComponentValueSetter;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentViewFactory;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class TableComponentView<T extends DocComponent>
        extends DocComponentView<TableComponent> {

    @Autowired
    DocComponentViewFactory viewFactory;
    @Autowired
    TableComponentValueSetter tableValueSetter;

    private TableComponentView() {//force spring creation
        componentViewType = ComponentViewType.TABLE;
    }

    @Override
    public void setComponent(TableComponent docComponent) {
        super.setComponent(docComponent);
    }

    @Override
    public void setComponentValue(Boolean value) {
        throw new DocGenException("Table component does not accept boolean values as value setter");
    }

    @Override
    public void setComponentValue(String value) {
        throw new DocGenException("Table component does not accept String values as value setter");
    }

    @Override
    public void checkAndSetValueFromParamString(String paramString, String value) {
        tableValueSetter.setCellValueIfMatch(paramString, value, this);
    }

    public List<TableRow> getTableRows() {
        return docComponent.getRows();
    }

    public List<TableCell> getHeaderCells(){
        TableHeaderRow headerRow = docComponent.getHeaderRow();
        return headerRow.getCells();
    }

    public boolean allowsProductInjection() {
        return true;
    }

    public void injectProducts(List<ProductView> products) {
        TableComponentProductInjector productInjector = new TableComponentProductInjector();
        productInjector.injectProducts(docComponent, products);
    }

    public DocComponentView getCellComponentView(int rowNum, int colNum) {
        TableRow row = docComponent.getRow(rowNum);
        TableCell cell = row.getCell(colNum);
        DocComponent component = cell.getComponent();
        return viewFactory.createComponentView(component);
    }
}
