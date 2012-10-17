package org.lawrencebower.docgen.web_logic.business.mapping;

import org.apache.commons.lang.StringUtils;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.lawrencebower.docgen.web_model.ViewConstants.DOCUMENT_FIELD_SEPARATOR;

public class FieldMapper {

    private static Set<String> EXCLUDED_TOKENS;

    static {
        EXCLUDED_TOKENS = new HashSet<>();
        EXCLUDED_TOKENS.add("full");
        EXCLUDED_TOKENS.add("partial");
    }

    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                           List<DocumentInfoView> documents) {

        for (String key : parameterMap.keySet()) {

            if (isExcludedToken(key)) {
                continue;
            }

            extractAndSetValues(parameterMap,
                                documents,
                                key);
        }

    }

    private void extractAndSetValues(Map<String, String[]> parameterMap,
                                     List<DocumentInfoView> documents,
                                     String key) {

        DocumentComponentPair documentAndComponentName = extractDocumentAndComponentName(key);

        String documentName = documentAndComponentName.getDocumentName();
        String componentName = documentAndComponentName.getFieldName();

        DocumentInfoView document = getDocument(documentName, documents);

        DocComponentView component = getComponent(componentName, document);

        String value = getFieldValue(key, parameterMap.get(key));

        setComponentValue(component, value);
    }

    private void setComponentValue(DocComponentView component, String value) {
        if (component.isTextComponent()) {
            ((TextComponent) component.getDocComponent()).setText(value);
        }
    }

    private String getFieldValue(String key, String[] strings) {

        if (strings.length == 0) {
            throw new DocGenException("No values bound to field - " + key);
        }

        if (strings.length > 1) {
            throw new DocGenException("more than 1 value bound to field - " + key);
        }

        return strings[0];
    }

    private DocComponentView getComponent(String componentName, DocumentInfoView document) {

        for (DocComponentView componentView : document.getComponentViews()) {
            String name = componentView.getName();
            if (name != null && name.equals(componentName)) {
                return componentView;
            }
        }

        throw new DocGenException("DocComponent not found " + componentName);
    }

    private DocumentInfoView getDocument(String documentName, List<DocumentInfoView> documents) {

        for (DocumentInfoView document : documents) {
            if (document.getName() != null && document.getName().equals(documentName)) {
                return document;
            }
        }

        throw new DocGenException("Document not found - " + documentName);
    }

    private DocumentComponentPair extractDocumentAndComponentName(String fieldName) {

        if (!fieldName.contains(DOCUMENT_FIELD_SEPARATOR)) {
            throw new DocGenException("Field name does not contain separator? " + fieldName);
        }

        String[] strings = StringUtils.split(fieldName, DOCUMENT_FIELD_SEPARATOR);

        if (strings.length != 2) {
            String message = String.format("Unexpected number of tokens (%s) when parsing field name, %s",
                                           strings.length,
                                           fieldName);

            throw new DocGenException(message);
        }

        return new DocumentComponentPair(strings[0], strings[1]);
    }

    public boolean isExcludedToken(String token) {
        return EXCLUDED_TOKENS.contains(token);
    }

    class DocumentComponentPair {

        private String documentName;
        private String fieldName;

        DocumentComponentPair(String documentName, String fieldName) {

            if (StringUtils.isWhitespace(documentName)) {
                throw new DocGenException("documentName is null " + documentName);
            }

            if (StringUtils.isWhitespace(fieldName)) {
                throw new DocGenException("fieldName is null " + documentName);
            }

            this.documentName = documentName;
            this.fieldName = fieldName;
        }

        public String getDocumentName() {
            return documentName;
        }

        public String getFieldName() {
            return fieldName;
        }
    }
}
