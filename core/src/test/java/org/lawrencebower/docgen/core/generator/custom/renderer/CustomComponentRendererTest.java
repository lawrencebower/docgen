package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;

import java.io.ByteArrayOutputStream;

import static org.junit.Assert.fail;

public class CustomComponentRendererTest {

    @Test
    public void testRenderComponent_passUnknownComponent_throwsError() throws Exception {

        CustomComponentRenderer renderer = new CustomComponentRenderer();

        DocComponent unknownComponent = getUnknownComponent();

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        try {
            renderer.renderComponent(unknownComponent, rendererInfo);
        } catch (DocGenException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testRenderComponent_passCheckboxComponent_throwsError() throws Exception {

        CustomComponentRenderer renderer = new CustomComponentRenderer();

        DocComponent checkboxComponent = new CheckBoxComponent("name");

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        try {
            renderer.renderComponent(checkboxComponent, rendererInfo);
        } catch (UnsupportedOperationException e) {
            return;
        }

        fail();//should not get here
    }

    private DocComponent getUnknownComponent() {
        return new DocComponent("made up component") {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };
    }
}
