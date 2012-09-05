package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.springframework.beans.factory.annotation.Autowired;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class OverlayComponentRenderer
        implements DocComponentRenderer<DocComponent, OverlayComponentRendererInfo, Element> {

    @Autowired
    private OverlayTextRenderer textRenderer;
    @Autowired
    private OverlayTableRenderer tableRenderer;
    @Autowired
    private OverlayCheckBoxRenderer checkBoxRenderer;
    @Autowired
    private OverlayImageRenderer imageRenderer;

    @Override
    public void createAndRenderComponent(DocComponent component, OverlayComponentRendererInfo rendererInfo) {

        if (component instanceof TextComponent) {
            textRenderer.createAndRenderComponent((TextComponent) component, rendererInfo);
        }else if (component instanceof TableComponent) {
            tableRenderer.createAndRenderComponent((TableComponent) component, rendererInfo);
        }else if (component instanceof CheckBoxComponent) {
            checkBoxRenderer.createAndRenderComponent((CheckBoxComponent) component, rendererInfo);
        }else if (component instanceof ImageComponent) {
            imageRenderer.createAndRenderComponent((ImageComponent) component, rendererInfo);
        }else if (component instanceof NewLineComponent) {
            throw new UnsupportedOperationException("NewLine not supported by Overlay renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }
    }

    @Override
    public Element createComponent(DocComponent component) {
        throw new NotImplementedException();
    }
}
