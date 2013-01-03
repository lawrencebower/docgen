package org.lawrencebower.docgen.web_logic.business.injection;

import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public interface TableComponentProductInjector {
    void injectProducts(TableComponent tableComponent, List<ProductView> products);
}
