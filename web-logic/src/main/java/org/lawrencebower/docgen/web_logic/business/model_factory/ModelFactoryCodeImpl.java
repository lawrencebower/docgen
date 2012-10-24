package org.lawrencebower.docgen.web_logic.business.model_factory;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_model.view.customer.Customer;
import org.lawrencebower.docgen.web_model.view.customer.CustomerView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import usecase.CommercialInvoice;
import usecase.DeliveryNote;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public class ModelFactoryCodeImpl implements ModelFactory {

    @Autowired
    private CommercialInvoice commercialInvoice;
    @Autowired
    private DeliveryNote deliveryNote;
    @Autowired
    private CustomerProduct_Document_Mappings customerProductDocMappings;

    private LinkedHashMap<String, CustomerView> customers = new LinkedHashMap<>();

    private LinkedHashMap<String, ProductView> products = new LinkedHashMap<>();

    private LinkedHashMap<String, DocumentInfoView> documents = new LinkedHashMap<>();

    private CustomerView customer1;
    private CustomerView customer2;
    private ProductView product1;
    private ProductView product2;
    private DocumentInfoView commercialInvoiceView;
    private DocumentInfoView deliveryNoteView;

    public void init() {
        initCustomers();
        initProducts();
        initDocuments();
        initCustomerProductDocumentMappings();
    }

    private void initDocuments() {

        commercialInvoiceView = commercialInvoice.getDocInfoView();

        deliveryNoteView = deliveryNote.getDocInfo();

        documents.put(commercialInvoiceView.getName(), commercialInvoiceView);
        documents.put(deliveryNoteView.getName(), deliveryNoteView);
    }

    private void initProducts() {

        product1 = new ProductView(new Product("id1", "name 1"));
        product2 = new ProductView(new Product("id2", "name 2"));

        products.put(product1.getId(), product1);
        products.put(product2.getId(), product2);
    }

    private void initCustomers() {

        customer1 = new CustomerView(new Customer("Customer 1",
                                                  "David Davidson",
                                                  "Just round the corner",
                                                  "198293893839",
                                                  "UK"));

        customer2 = new CustomerView(new Customer("Customer 2",
                                                  "Billy Bob Bobson",
                                                  "miles away",
                                                  "38783478347",
                                                  "CHINA"));

        customers.put(customer1.getCustomerName(), customer1);
        customers.put(customer2.getCustomerName(), customer2);
    }

    private void initCustomerProductDocumentMappings() {

        //Customer 1
        customerProductDocMappings.addDocument(customer1.getCustomer(),
                                               product1.getproduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer1.getCustomer(),
                                               product2.getproduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer1.getCustomer(),
                                               product2.getproduct(),
                                               deliveryNoteView);

        //Customer 2
        customerProductDocMappings.addDocument(customer2.getCustomer(),
                                               product1.getproduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer2.getCustomer(),
                                               product2.getproduct(),
                                               commercialInvoiceView);

        customerProductDocMappings.addDocument(customer2.getCustomer(),
                                               product2.getproduct(),
                                               deliveryNoteView);

    }

    @Override
    public List<CustomerView> getCustomers() {
        return new ArrayList<>(customers.values());
    }

    @Override
    public List<ProductView> getProducts() {
        return new ArrayList<>(products.values());
    }

    public List<DocumentInfoView> getDocuments() {
        return new ArrayList<>(documents.values());
    }

    public CustomerView getCustomer(String customerName) {
        if (!customers.containsKey(customerName)) {
            throw new DocGenException(String.format("Customer %s not found?!", customerName));
        }
        return customers.get(customerName);
    }

    @Override
    public ProductView getProduct(String productId) {
        if (!products.containsKey(productId)) {
            throw new DocGenException(String.format("product %s not found?!", productId));
        }
        return products.get(productId);
    }
}
