package org.lawrencebower.docgen.web_model.view.document;

import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.business_def.mapping.field_value.FieldMapper;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewableComponentFilter;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

public class DocumentSetImpl implements DocumentSet {

    @Autowired(required = false)
    FieldMapper fieldMapper;
    @Autowired(required = false)
    ViewUtils viewUtils;
    @Autowired(required = false)
    private ViewableComponentFilter viewableComponentFilter;

    @Autowired
    DocumentViewFactory documentViewFactory;
    @Autowired
    DocumentSetFactory documentSetFactory;

    private List<DocumentView> documents = new ArrayList<>();

    private DocumentSetImpl() {//force spring creation
    }

    @Override
    public void setDocuments(Collection<DocumentView> documents) {
        this.documents = new ArrayList<>(documents);
    }

    @Override
    public void setDocuments(DocumentView... views) {
        List<DocumentView> docList = Arrays.asList(views);
        documents = new ArrayList<>(docList);
    }

    @Override
    public List<DocumentView> getDocumentsAsList() {
        return Collections.unmodifiableList(documents);
    }

    @Override
    public List<PDFDocument> createPDFs() {

        List<PDFDocument> results = new ArrayList<>();

        for (DocumentView document : documents) {

            PDFDocument pdfDocument = document.generatePDF();

            String documentName = document.getName();
            pdfDocument.setName(documentName);

            String nameExtension = document.getNameExtension();
            pdfDocument.setNameExtension(nameExtension);

            int copyNumber = document.getCopyNumber();
            pdfDocument.setCopyNumber(copyNumber);

            results.add(pdfDocument);
        }

        return results;
    }

    @Override
    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap) {
        List<DocComponentView> allComponentViews = getAllComponentViewsFromDocs();
        fieldMapper.mapFieldValuesToComponents(parameterMap, allComponentViews);
    }

    @Override
    public List<DocComponentView> getAllComponentViewsFromDocs() {
        return viewUtils.getAllComponentViewsFromDocs(documents);
    }

    @Override
    public void checkDocumentsSet() {
        viewUtils.checkDocumentsSet(documents);
    }

    @Override
    public void mapAutomappedComponents(AMComponentInfo mappingInfo) {

        checkDocumentsSet();

        List<DocComponentView> components = getAllComponentViewsFromDocs();

        for (DocComponentView component : components) {
            component.mapComponentValue(mappingInfo);
        }
    }

    @Override
    public void injectProductFields(List<ProductView> selectedProducts) {

        List<DocComponentView> componentViews = getAllComponentViewsFromDocs();

        for (DocComponentView componentView : componentViews) {
            componentView.injectProducts(selectedProducts);
        }
    }

    @Override
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

    @Override
    public void processCalculatedFields() {

        List<DocComponentView> componentViews = getAllComponentViewsFromDocs();

        for (DocComponentView componentView : componentViews) {
            componentView.calculateValueIfNeeded(this);
        }

    }

    @Override
    public void runCalculation(ComponentCalculation calculation) {
        calculation.runOnOperands(this);
    }

    @Override
    public DocumentSet injectDocuments(List<DocumentInjectionInfo> injectionInfos) {

        List<DocumentView> results = new ArrayList<>();

        for (DocumentView document : documents) {
            if (document.hasDocumentInjectionFields()) {
                List<DocumentView> injectedDocuments = document.injectDocuments(injectionInfos);
                results.addAll(injectedDocuments);
            } else {
                results.add(document);//just keep original originalDocument
            }
        }

        DocumentSet injectedDocumentSet = documentSetFactory.createDocumentInfoSet();
        injectedDocumentSet.setDocuments(results);

        return injectedDocumentSet;
    }
}
