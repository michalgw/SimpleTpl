<html>
   <body>
      <h1>{{title}}</h1>
      <p>Name: {{addressbook.name}}</p>
      <p>Address: <br>
      Street: {{addressbook.address.street}}<br>
      City: {{addressbook.address.city}}<br>
      zip: {{addressbook.address.zip}}</p>
      {{if &ArrayLen('addressbook.phoneNumbers') > 0}}
      <p>Phone numbers:
      <ul>
         {{loop addressbook.phoneNumbers|i}}
            <li>#{{&i + 1}}: {{addressbook.phoneNumbers[i]}}</li>
         {{endloop}}
      </ul>
      </p>
      {{endif}}
      <p>Expression: 2+2 = <strong>{{&2+2}}</strong><br>
      Zip + 123 = {{&AsInteger('addressbook.address.zip')+123}}
      </p>
      <p>
      {{%
         AddVarT('x', 'I'); SetVarI('x', 10-2-3);
         SetVarI('x', x+2+2+5*2);
      }}
      {{&IntToStr(x)}}
      {{%DelVar('x')}}
      </p>
      {{%AddVarT('SumNum', 'I')}}
      {{loop master|i}}
      <p>
      Name: <strong>{{master[i].name}}</strong><br>
      Number: <strong>{{master[i].number}}</strong><br>
      {{%SetVarI('SumNum', SumNum + AsInteger('master[i].number'))}}
      Detail:<br>
         {{loop master[i].detail|d}}
            {{&d+1}} - <strong>{{master[i].detail[d]}}</strong>{{if & d + 1 < ArrayLen('master[i].detail')}}<br>{{endif}}
         {{endloop}}
      </p>
      {{endloop}}
      <p>Sum number: {{&SumNum}}</p>
      <p>Expression loop</p>
      <p>
      <table>
      {{%AddVarT('x', 'I')}}
      {{loop &10|||i}}
         <tr>
         {{loop &10|||j}}
            {{%SetVarI('x',(i+1)*(j+1))}}<td style="background-color: {{&
               if(x<10,'lime','');
               if((x>=10) and (x<25),'green','');
               if((x>=25) and (x<50),'yellow','');
               if((x>=50) and (x<75),'brown','');
               if((x>=75) and (x<100),'red','');
               if(x>=100,'purple','')
               }}">{{&x}}</td>
         {{endloop}}
         </tr>
      {{endloop}}
      </table>
      </p>
      <p>
      <table>
      {{loop &||i >= 4|i}}
         <tr>
         {{loop &|j >= 6||j}}
            <td>{{&(i+1)*(j+1)}}</td>
         {{endloop}}
         </tr>
      {{endloop}}
      </table>
      </p>
      int: {{test.int}} AsInteger: {{&AsInteger('test.int')}}<br>
      float: {{test.float}} AsFloat: {{&AsFloat('test.float')}}<br>
      date: {{test.date}} AsDateTime: {{&AsDateTime('test.date')}}<br>
      datetime: {{test.datetime}} AsDateTime: {{&AsDateTime('test.datetime')}}<br>
      string: {{test.string}} AsString: {{&AsString('test.string')}}<br>
      class: ArrayLen: {{&ArrayLen('test.class')}}<br>
      array: ArrayLen: {{&ArrayLen('test.array')}}<br>
      null: IsNull: {{&IsNull('test.null')}}<br>
      bool true: {{test.boolt}} AsBoolean: {{&AsBoolean('test.boolt')}}<br>
      bool false: {{test.boolf}} AsBoolean: {{&AsBoolean('test.boolf')}}<br>
   </body>
</html>