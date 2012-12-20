package org.lawrencebower.docgen.web_logic.view.model_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.model_factory.factory.*;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class ViewFactoryImpl implements ViewFactory {

    private CustomerProduct_Document_Mappings customerProductDocMappings;

    private Map<String, ContactView> customers;

    private Map<String, ContactView> businesses;

    private Map<String, ProductView> products;

    private ContactView vendor;

    @Autowired
    private VendorFactory vendorFactory;
    @Autowired
    private ProductFactory productFactory;
    @Autowired
    private CustomerFactory customerFactory;
    @Autowired
    private CustomerProductMappingFactory customerProductFactory;
    @Autowired
    private DocumentFactory documentFactory;

    public void init() {
        initVendor();
        initCustomers();
        initBusinesses();
        initProducts();
        initCustomerProductDocumentMappings();
    }

    private void initVendor() {
        vendor = vendorFactory.getVendor();
    }

    private void initProducts() {
        products = productFactory.getProducts();
    }

    private void initCustomers() {
        customers = customerFactory.getCustomers();
    }

    private void initBusinesses() {
        businesses = customerFactory.getBusinesses();
    }

    private void initCustomerProductDocumentMappings() {
        customerProductDocMappings = customerProductFactory.getMappingInfo();
    }

    @Override
    public List<ContactView> getCustomers() {
        Collection<ContactView> contactViews = customers.values();
        return new ArrayList<>(contactViews);
    }

    @Override
    public ArrayList<ContactView> getBusinesses() {
        Collection<ContactView> contactViews = businesses.values();
        return new ArrayList<>(contactViews);
    }

    @Override
    public List<DocumentView> getDocumentsForCustomerAndProduct(ContactView customer, ProductView product) {

        List<String> documentNames =
                customerProductDocMappings.getDocumentsForCustomerAndProduct(customer, product);

        List<DocumentView> results = new ArrayList<>();

        for (String documentName : documentNames) {
            DocumentView documentView = createDocument(documentName);
            results.add(documentView);
        }

        return results;
    }

    @Override
    public List<ProductView> getProducts() {

        List<ProductView> results = new ArrayList<>();
        for (ProductView product : products.values()) {
            results.add(product);
        }

        return results;
    }

    public List<DocumentView> getAllDocuments() {
        return documentFactory.getAllDocuments();
    }

    @Override
    public DocumentView createDocument(String documentName) {
        return documentFactory.createDocument(documentName);
    }

    public ContactView getCustomer(String customerName) {
        if (!customers.containsKey(customerName)) {
            String message = String.format("Contact %s not found?!", customerName);
            throw new DocGenException(message);
        }
        return customers.get(customerName);
    }

    @Override
    public ContactView getBusinessByCustomerName(String customerName) {
        if (!businesses.containsKey(customerName)) {
            String message = String.format("Business %s not found?!", customerName);
            throw new DocGenException(message);
        }
        return businesses.get(customerName);
    }

    @Override
    public ProductView getProduct(String productId) {
        if (!products.containsKey(productId)) {
            String message = String.format("product '%s' not found?!", productId);
            throw new DocGenException(message);
        }
        return products.get(productId);
    }

    @Override
    public ContactView getVendor() {
        return vendor;
    }
}
