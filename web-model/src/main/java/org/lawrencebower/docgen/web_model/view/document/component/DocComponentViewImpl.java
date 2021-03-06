package org.lawrencebower.docgen.web_model.view.document.component;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.AutoMapped;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionField;
import org.lawrencebower.docgen.web_model.view.document.binding.DocComponentBindBean;
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
    public boolean isAutoMapped() {
        String componentName = getName();
        return autoMapper.matchesName(componentName);
    }

    @Override
    public boolean isText() {
        return false;
    }

    @Override
    public boolean isTextArea() {
        return false;
    }

    @Override
    public boolean isTable() {
        return false;
    }

    protected ComponentViewType getComponentViewType() {
        return componentViewType;
    }

    @Override
    public void setRenderBorder(boolean renderBorder) {
        docComponent.setRenderBorder(renderBorder);
    }

    @Override
    public void checkAndSetValueFromBindBean(DocComponentBindBean bindBean) {

        String thisComponentName = getName();
        String bindName = bindBean.getName();
        String bindValue = bindBean.getValue();

        if (bindName.equals(thisComponentName)) {
            setComponentValue(bindValue);
        }
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
        String name = this.getName();
        builder.append(name);
        return builder.toHashCode();
    }
}
