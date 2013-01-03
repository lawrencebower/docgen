package org.lawrencebower.docgen.web_model.business_def.injection;

import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface TableComponentProductInjector {
    void injectProducts(TableComponent tableComponent, List<ProductView> products);
}
