package org.lawrencebower.docgen.core.document.component.position;

import com.lowagie.text.Element;
import org.junit.Test;

import static junit.framework.Assert.assertEquals;

public class DocAlignmentTest {

    @Test
    public void testMapToITextAlignment_left_returnsITextLeft() throws Exception {
        int iTextAlignment = mapAlignment(DocAlignment.LEFT);
        assertEquals(Element.ALIGN_LEFT, iTextAlignment);
    }

    @Test
    public void testMapToITextAlignment_right_returnsITextRight() throws Exception {
        int iTextAlignment = mapAlignment(DocAlignment.RIGHT);
        assertEquals(Element.ALIGN_RIGHT, iTextAlignment);
    }

    @Test
    public void testMapToITextAlignment_center_returnsITextCenter() throws Exception {
        int iTextAlignment = mapAlignment(DocAlignment.CENTER);
        assertEquals(Element.ALIGN_CENTER, iTextAlignment);
    }

    @Test
    public void testMapToITextAlignment_justified_returnsITextJustified() throws Exception {
        int iTextAlignment = mapAlignment(DocAlignment.JUSTIFIED);
        assertEquals(Element.ALIGN_JUSTIFIED, iTextAlignment);
    }

    private int mapAlignment(DocAlignment alignment) {
        return DocAlignment.mapToITextAlignment(alignment);
    }
}
