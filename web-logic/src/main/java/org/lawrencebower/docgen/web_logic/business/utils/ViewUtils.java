package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ViewUtils {

    public static final String NO_CUSTOMER_SELECTED = "No customer selected?!";
    public static final String NO_PRODUCTS_SELECTED = "No products selected?!";
    public static final String NO_BUSINESS_SELECTED = "No business selected?!";
    public static final String NO_DOCUMENTS_SELECTED = "No documents selected?!";

    public List<DocComponentView> getAllComponentViewsFromDocs(List<DocumentInfoView> documents) {

        List<DocComponentView> results = new ArrayList<>();

        for (DocumentInfoView documentInfoView : documents) {
            List<DocComponentView> docComponentViews = documentInfoView.getComponentViews();
            results.addAll(docComponentViews);
        }

        return results;
    }

    public void checkBusinessSet(ContactView selectedBusiness) {
        if (selectedBusiness == null) {
            throw new DocGenException(NO_BUSINESS_SELECTED);
        }
    }

    public void checkProductsSet(List<ProductView> selectedProducts) {
        if ((selectedProducts == null) || selectedProducts.isEmpty()) {
            throw new DocGenException(NO_PRODUCTS_SELECTED);
        }
    }

    public void checkDocumentsSet(List<DocumentInfoView> selectedDocuments) {
        if ((selectedDocuments == null) || selectedDocuments.isEmpty()) {
            throw new DocGenException(NO_DOCUMENTS_SELECTED);
        }
    }

    public void checkCustomerSet(ContactView selectedCustomer) {
        if (selectedCustomer == null) {
            throw new DocGenException(NO_CUSTOMER_SELECTED);
        }
    }

    /**
     * returns DocumentComponentView with name matching the provided value. Only the first componentView
     * that matches is returned
     */
    public List<Float> getOperandValuesByName(String operandName,
                                              List<DocumentInfoView> allDocs) {

        for (DocumentInfoView docView : allDocs) {
            if (docView.hasComponentViewWithName(operandName)) {
                DocComponentView componentView = docView.getComponentViewWithName(operandName);
                Float value = componentView.getFloatValue();
                return Arrays.asList(value);
            }else if(docView.hasTableWithColumnName(operandName)){
                return docView.getTableColumnValuesAsFloats(operandName);
            }
        }

        String message = String.format("Could not find DocComponent with name '%s'", operandName);
        throw new DocGenException(message);
    }

    public List<DocComponentView> getAllDocComponentViewsWithName(String componentName,
                                                                  List<DocumentInfoView> allDocs) {
        List<DocComponentView> results = new ArrayList<>();

        for (DocumentInfoView docView : allDocs) {
            if (docView.hasComponentViewWithName(componentName)) {
                DocComponentView componentView = docView.getComponentViewWithName(componentName);
                results.add(componentView);
            }
        }

        return results;
    }
}
