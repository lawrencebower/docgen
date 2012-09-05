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

    @Override
    public void createAndRenderComponent(DocComponent component, CustomComponentRendererInfo rendererInfo) {

        if (component instanceof TextComponent) {
            textRenderer.createAndRenderComponent((TextComponent) component, rendererInfo);
        } else if (component instanceof TableComponent) {
            tableRenderer.createAndRenderComponent((TableComponent) component, rendererInfo);
        } else if (component instanceof NewLineComponent) {
            newLineRenderer.createAndRenderComponent((NewLineComponent) component, rendererInfo);
        } else if (component instanceof ImageComponent) {
            imageRenderer.createAndRenderComponent((ImageComponent) component, rendererInfo);
        } else if (component instanceof CheckBoxComponent) {
            throw new UnsupportedOperationException("Check box not supported by Custom renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }
    }

    @Override
    public Element createComponent(DocComponent component) {

        Element element;

        if (component instanceof TextComponent) {
            element = textRenderer.createComponent((TextComponent) component);
        } else if (component instanceof TableComponent) {
            element = tableRenderer.createComponent((TableComponent) component);
        } else if (component instanceof NewLineComponent) {
            element = newLineRenderer.createComponent((NewLineComponent) component);
        } else if (component instanceof ImageComponent) {
            element = imageRenderer.createComponent((ImageComponent) component);
        } else if (component instanceof CheckBoxComponent) {
            throw new UnsupportedOperationException("Check box not supported by Custom renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }

        return element;
    }
}
