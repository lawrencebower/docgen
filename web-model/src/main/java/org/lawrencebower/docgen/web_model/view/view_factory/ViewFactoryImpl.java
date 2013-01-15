package org.lawrencebower.docgen.web_model.view.view_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class ViewFactoryImpl implements ViewFactory {

    private CustomerProduct_Document_Mappings customerProductDocMappings;

    private Map<String, ContactView> customers;

    private Map<String, String> businesses;

    private Map<String, ProductView> products;

    private ContactView vendor;

    @Autowired(required = false)
    private VendorFactory vendorFactory;
    @Autowired(required = false)
    private ProductFactory productFactory;
    @Autowired(required = false)
    private CustomerFactory customerFactory;
    @Autowired(required = false)
    private BusinessFactory businessFactory;
    @Autowired(required = false)
    private CustomerProductMappingFactory customerProductFactory;
    @Autowired(required = false)
    private DocumentFactory documentFactory;

    public void init() {
        vendor = vendorFactory.getVendor();
        products = productFactory.getProducts();
        customers = customerFactory.getCustomers();
        businesses = businessFactory.getBusinesses();

        List<ContactView> allCustomers = getCustomers();
        List<ProductView> allProducts = getProducts();
        List<DocumentView> allDocuments = getAllDocuments();

        customerProductDocMappings = customerProductFactory.getMappingInfo(allCustomers,
                                                                           allProducts,
                                                                           allDocuments);
    }

    @Override
    public List<ContactView> getCustomers() {
        Collection<ContactView> contactViews = customers.values();
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
    public DocumentViewImpl createDocument(String documentName) {
        return documentFactory.createDocument(documentName);
    }

    public ContactView getContact(String contactId) {

        if (!customers.containsKey(contactId)) {
            String message = String.format("Contact %s not found?!", contactId);
            throw new DocGenException(message);
        }

        return customers.get(contactId);
    }

    @Override
    public ContactView getBusinessByCustomerName(String customerContactId) {

        ContactView customer;

        if (!businesses.containsKey(customerContactId)) {
            customer = getContact(customerContactId);//use the same customer if no business mapped
        } else {
            String businessId = businesses.get(customerContactId);
            customer = getContact(businessId);
        }

        return customer;
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

    public void setCustomerProductFactory(CustomerProductMappingFactory customerProductFactory) {
        this.customerProductFactory = customerProductFactory;
    }

    public void setDocumentFactory(DocumentFactory documentFactory) {
        this.documentFactory = documentFactory;
    }

    public void setVendorFactory(VendorFactory vendorFactory) {
        this.vendorFactory = vendorFactory;
    }

    public void setProductFactory(ProductFactory productFactory) {
        this.productFactory = productFactory;
    }

    public void setCustomerFactory(CustomerFactory customerFactory) {
        this.customerFactory = customerFactory;
    }

    public void setBusinessFactory(BusinessFactory businessFactory) {
        this.businessFactory = businessFactory;
    }
}
