package org.lawrencebower.docgen.web_logic.business.xml_mapper;

import org.lawrencebower.docgen.web_logic.view.customer.CustomerView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public interface ModelFactory {

    public List<CustomerView> getCustomers();

    List<DocumentInfoView> getDocuments();

    List<ProductView> getProducts();

    CustomerView getCustomer(String customerName);

    ProductView getProduct(String productId);
}
