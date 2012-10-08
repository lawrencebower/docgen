package org.lawrencebower.docgen.web_logic.business.controler_business;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.model.customer.Customer;
import org.lawrencebower.docgen.web_logic.model.product.Product;
import org.lawrencebower.docgen.web_logic.view.customer.CustomerView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DataEntryCB {

    @Autowired
    private CustomerProduct_Document_Mappings mappings;

    public Set<DocumentInfo> prepareFieldsForViewing(CustomerView selectedCustomer,
                                                     List<ProductView> selectedProducts) {

        Customer customer = selectedCustomer.getCustomer();
        Set<DocumentInfo> docInfos = new HashSet<>();

        for (ProductView selectedProduct : selectedProducts) {
            Product product = selectedProduct.getproduct();
            List<DocumentInfo> docInfo = mappings.getDocInfosForCustomerAndProduct(customer, product);
            docInfos.addAll(docInfo);
        }

        return docInfos;
    }
}
