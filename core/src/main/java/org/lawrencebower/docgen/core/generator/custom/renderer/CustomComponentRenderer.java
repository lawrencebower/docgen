package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomComponentRenderer
        implements DocComponentRenderer<DocComponent, CustomComponentRendererInfo, Element> {

    @Autowired
    private CustomTextRenderer textRenderer;
    @Autowired
    private CustomTableRenderer tableRenderer;
    @Autowired
    private CustomNewLineRenderer newLineRenderer;
    @Autowired
    private CustomImageRenderer imageRenderer;
    @Autowired
    private CustomTableTextRenderer tableTextRenderer;
    @Autowired
    private CustomLineRenderer lineRenderer;

    @Override
    public void createAndRenderComponent(DocComponent component, CustomComponentRendererInfo rendererInfo) {

        if (component.getComponentType() == DocComponentType.TEXT) {
            textRenderer.createAndRenderComponent((TextComponent) component, rendererInfo);
        } else if (component.getComponentType() == DocComponentType.TABLE) {
            tableRenderer.createAndRenderComponent((TableComponent) component, rendererInfo);
        } else if (component.getComponentType() == DocComponentType.NEWLINE) {
            newLineRenderer.createAndRenderComponent((NewLineComponent) component, rendererInfo);
        } else if (component.getComponentType() == DocComponentType.IMAGE) {
            imageRenderer.createAndRenderComponent((ImageComponent) component, rendererInfo);
        } else if (component.getComponentType() == DocComponentType.TABLE_TEXT) {
            tableTextRenderer.createAndRenderComponent((TableTextComponent) component, rendererInfo);
        } else if (component.getComponentType() == DocComponentType.LINE) {
            lineRenderer.createAndRenderComponent((LineComponent) component, rendererInfo);
        } else if (component.getComponentType() == DocComponentType.CHECKBOX) {
            throw new UnsupportedOperationException("Check box not supported by Custom renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }
    }

    @Override
    public Element createComponent(DocComponent component) {

        Element element;

        if (component.getComponentType() == DocComponentType.TEXT) {
            element = textRenderer.createComponent((TextComponent) component);
        } else if (component.getComponentType() == DocComponentType.TABLE) {
            element = tableRenderer.createComponent((TableComponent) component);
        } else if (component.getComponentType() == DocComponentType.NEWLINE) {
            element = newLineRenderer.createComponent((NewLineComponent) component);
        } else if (component.getComponentType() == DocComponentType.IMAGE) {
            element = imageRenderer.createComponent((ImageComponent) component);
        } else if (component.getComponentType() == DocComponentType.TABLE_TEXT) {
            element = tableTextRenderer.createComponent((TextComponent) component);
        } else if (component.getComponentType() == DocComponentType.LINE) {
            element = lineRenderer.createComponent((LineComponent) component);
        } else if (component.getComponentType() == DocComponentType.CHECKBOX) {
            throw new UnsupportedOperationException("Check box not supported by Custom renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }

        return element;
    }
}
