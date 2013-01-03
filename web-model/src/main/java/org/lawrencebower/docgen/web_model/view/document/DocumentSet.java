package org.lawrencebower.docgen.web_model.view.document;

import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public interface DocumentSet {
    void setDocuments(Collection<DocumentView> documents);

    void setDocuments(DocumentView... views);

    List<DocumentView> getDocumentsAsList();

    List<PDFDocument> createPDFs();

    void mapFieldValuesToComponents(Map<String, String[]> parameterMap);

    List<DocComponentView> getAllComponentViewsFromDocs();

    void checkDocumentsSet();

    void mapAutomappedComponents(AMComponentInfo mappingInfo);

    void injectProductFields(List<ProductView> selectedProducts);

    List<DocComponentView> getComponentsForViewing(boolean showAutoMappedFields);

    void processCalculatedFields();

    void runCalculation(ComponentCalculation calculation);

    DocumentSet injectDocuments(List<DocumentInjectionInfo> injectionInfos);
}
