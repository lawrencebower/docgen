package org.lawrencebower.docgen.web_model.view.document.component;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.AutoMapped;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionField;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class DocComponentViewImpl<T extends DocComponent> implements DocComponentView<T> {

    @Autowired(required = false)
    private AutoMapped autoMapper;

    protected static final String NULL_COMPONENT_MESSAGE = "DocComponent is null";
    protected static final String NOT_SET_MESSAGE = "not set";

    public enum ComponentViewType {
        TEXT,
        TEXT_AREA,
        CHECK_BOX,
        TABLE,
        CV
    }

    protected T docComponent;
    protected ComponentViewType componentViewType;

    @Override
    public boolean isDocumentInjection() {
        String componentName = getName();
        return DocumentInjectionField.containsName(componentName);
    }

    @Override
    public void setComponent(T docComponent) {
        if (docComponent == null) {
            throw new DocGenException(NULL_COMPONENT_MESSAGE);
        }

        this.docComponent = docComponent;
    }

    @Override
    public String getName() {

        String name = NOT_SET_MESSAGE;

        if (docComponent.getName() != null) {
            name = docComponent.getName();
        }

        return name;
    }

    @Override
    public void mapComponentValue(AMComponentInfo mappingInfo) {
        if (isAutoMapped()) {
            autoMapper.mapComponent(this, mappingInfo);
        }
    }

    @Override
    public boolean isAutoMapped(){
        String componentName = getName();
        return autoMapper.matchesName(componentName);
    }

    @Override
    public boolean isText() {
        return componentViewType == ComponentViewType.TEXT;
    }

    @Override
    public boolean isTextArea() {
        return componentViewType == ComponentViewType.TEXT_AREA;
    }

    @Override
    public boolean isTable() {
        return componentViewType == ComponentViewType.TABLE;
    }

    @Override
    public void setRenderBorder(boolean renderBorder) {
        docComponent.setRenderBorder(renderBorder);
    }

    @Override
    public boolean equals(Object obj) {

        if (!(obj instanceof DocComponentViewImpl)) {
            return false;
        }

        DocComponentView compareTo = (DocComponentView) obj;
        EqualsBuilder builder = new EqualsBuilder();
        builder.append(this.getName(), compareTo.getName());

        return builder.isEquals();
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(this.getName());
        return builder.toHashCode();
    }
}
