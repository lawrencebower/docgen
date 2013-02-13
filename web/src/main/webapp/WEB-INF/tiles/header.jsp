<%@ include file="/WEB-INF/jsp/include.jsp" %>
<h1><fmt:message key="globalMessages.heading"/></h1>
<s:url var="clear_session_url" value="/clearSession"/>
<a href="${clear_session_url}">Clear and start new</a>
<br/>
<br/>