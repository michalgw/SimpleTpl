<html>
	<head>
		<title>Table test</title>
	</head>
	<body>
		<table>
			<tr style="font-weight: bold; ">
			{{loop column}}
				<td>{{column_name}}</td>
			{{endloop}}
			</tr>
			{{loop row}}
			<tr{{if oddrow}} style="background-color: gray; "{{endif}}>
				{{loop column}}
					<td>{{value}}</td>
				{{endloop}}
			</tr>
			{{endloop}}
		</table>
	</body>
</html>