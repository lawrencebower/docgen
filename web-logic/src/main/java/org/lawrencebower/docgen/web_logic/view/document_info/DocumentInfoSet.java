package org.lawrencebower.docgen.web_logic.view.document_info;

import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.ViewableComponentFilter;
import org.lawrencebower.docgen.web_logic.business.mapping.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.FieldMapper;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

public class DocumentInfoSet {

    @Autowired
    FieldMapper fieldMapper;
    @Autowired
    ViewUtils viewUtils;
    @Autowired
    private ViewableComponentFilter viewableComponentFilter;

    private List<DocumentInfoView> documents = new ArrayList<>();

    private DocumentInfoSet() {//force spring creation
    }

    public void setDocuments(Collection<DocumentInfoView> documents) {
        this.documents = new ArrayList<>(documents);
    }

    public void setDocuments(DocumentInfoView... views) {
        List<DocumentInfoView> docList = Arrays.asList(views);
        documents = new ArrayList<>(docList);
    }

    public List<DocumentInfoView> getDocumentsAsList() {
        return Collections.unmodifiableList(documents);
    }

    public List<PDFDocument> createPDFs() {

        List<PDFDocument> results = new ArrayList<>();

        for (DocumentInfoView document : documents) {
            PDFDocument pdfDocument = document.generatePDF();
            String documentName = document.getName();
            pdfDocument.setName(documentName);
            results.add(pdfDocument);
        }

        return results;
    }

    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap) {
        List<DocComponentView> allComponentViews = getAllComponentViewsFromDocs();
        fieldMapper.mapFieldValuesToComponents(parameterMap, allComponentViews);
    }

    public List<DocComponentView> getAllComponentViewsFromDocs() {
        return viewUtils.getAllComponentViewsFromDocs(documents);
    }

    public void checkDocumentsSet() {
        viewUtils.checkDocumentsSet(documents);
    }

    public void mapAutomappedComponents(AutoMappedComponentInfo mappingInfo) {

        checkDocumentsSet();

        List<DocComponentView> components = getAllComponentViewsFromDocs();

        for (DocComponentView component : components) {
            component.mapComponentValue(mappingInfo);
        }
    }

    public void injectProductFields(List<ProductView> selectedProducts) {

        List<DocComponentView> componentViews = getAllComponentViewsFromDocs();

        for (DocComponentView componentView : componentViews) {
            componentView.injectProducts(selectedProducts);
        }
    }

    public List<DocComponentView> getComponentsForViewing(boolean showAutoMappedFields) {

        List<DocComponentView> results;

        List<DocComponentView> allComponentViews = getAllComponentViewsFromDocs();

        if (showAutoMappedFields) {
            results = viewableComponentFilter.getComponents(allComponentViews);
        } else {
            results = viewableComponentFilter.getNonAutoMappedComponents(allComponentViews);
        }

        return results;
    }

    public void processCalculatedFields() {

        List<DocComponentView> componentViews = getAllComponentViewsFromDocs();

        for (DocComponentView componentView : componentViews) {
            componentView.calculateValueIfNeeded(this);
        }

    }

    public void runCalculation(ComponentCalculation calculation) {
        calculation.runOnOperands(this);
    }

}
