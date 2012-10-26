package org.lawrencebower.docgen.web_logic.business.model_factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public interface ModelFactory {

    public List<ContactView> getCustomers();

    List<DocumentInfoView> getDocuments();

    List<ProductView> getProducts();

    ContactView getCustomer(String customerName);

    ProductView getProduct(String productId);

    ContactView getVendor();

    ContactView getBusinessByCustomerName(String customerName);

    ArrayList<ContactView> getBusinesses();
}
