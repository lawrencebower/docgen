package org.lawrencebower.docgen.web_model.view.view_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class ViewFactoryImpl implements ViewFactory {

    @Autowired(required = false)
    private VendorFactory vendorFactory;
    @Autowired(required = false)
    private ProductFactory productFactory;
    @Autowired(required = false)
    private CustomerFactory customerFactory;
    @Autowired(required = false)
    private BusinessFactory businessFactory;
    @Autowired(required = false)
    private DocumentFactory documentFactory;
    @Autowired
    private CustomerProductMappingFactory customerProductFactory;

    public void init() {

        List<ContactView> allCustomers = getCustomers();
        List<ProductView> allProducts = getProducts();
        List<DocumentView> allDocuments = getAllDocuments();

        customerProductFactory.initMappingInfo(allCustomers,
                                               allProducts,
                                               allDocuments);
    }

    @Override
    public List<ContactView> getCustomers() {
        return customerFactory.getCustomersAsList();
    }

    @Override
    public List<DocumentView> getDocumentsForCustomerAndProduct(ContactView customer, ProductView product) {

        List<String> documentNames =
                customerProductFactory.getDocumentsForCustomerAndProduct(customer, product);

        List<DocumentView> results = new ArrayList<>();

        for (String documentName : documentNames) {
            DocumentView documentView = createDocument(documentName);
            results.add(documentView);
        }

        return results;
    }

    @Override
    public void reloadData() {
        System.out.println("ViewFactoryImpl.reloadData");
        customerFactory.reloadCustomers();
        businessFactory.reloadBusiness();
        vendorFactory.reloadVendor();
        productFactory.reloadProducts();
        init();
    }

    @Override
    public List<ProductView> getProducts() {
        return productFactory.getProductsAsList();
    }

    public List<DocumentView> getAllDocuments() {
        return documentFactory.getAllDocuments();
    }

    @Override
    public DocumentView createDocument(String documentName) {
        return documentFactory.createDocument(documentName);
    }

    public ContactView getContact(String contactId) {

        if (!customerFactory.hasCustomer(contactId)) {
            String message = String.format("Contact %s not found?!", contactId);
            throw new DocGenException(message);
        }

        return customerFactory.getCustomer(contactId);
    }

    @Override
    public ContactView getBusinessByCustomerName(String customerContactId) {

        ContactView customer;

        if (!businessFactory.containsContactId(customerContactId)) {
            customer = getContact(customerContactId);//use the same customer if no business mapped
        } else {
            String businessId = businessFactory.getMappedBusiness(customerContactId);
            customer = getContact(businessId);
        }

        return customer;
    }

    @Override
    public ProductView getProduct(String productId) {
        if (!productFactory.hasProduct(productId)) {
            String message = String.format("product '%s' not found?!", productId);
            throw new DocGenException(message);
        }

        return productFactory.getProduct(productId);
    }

    @Override
    public ContactView getVendor() {
        return vendorFactory.getVendor();
    }
}
