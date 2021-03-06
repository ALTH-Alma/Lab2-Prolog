
%_________________________________________
%Algunas espicificaciones: 

%Para la representación de las fechas en los TDAs respuestas y preguntas se usa la siguiente estructura: [entero dia, entero mes,entero año],
%donde dia es un entero mayor que 0 y menor que la cantidad de dias del mes, mes es un entero entre 1 y 12, y año es un entero.

%Para la representación de la recompensa en preguntas se usa la siguiente estructura: [string nombre oferto, entero monto recompensa], aquí se almacena
%temporalmente el puntaje del usuario que ofrece la recompensa. Una recompensa vacía o no recompensa se representa como ["",0].

%Para la listas de referencias y etiquetas en los TDAs se usa simplemente una lista de strings.
%_________________________________________
%Predicado extra: verifica que una lista sea de strings. Se usa para verificar el correcto uso para listas de referencias y etiquetas.
%Entradas: [H|T]: una lista de strings, H: string en cabeza, T:cola de strings.
%Meta: un booleano.

esListaString([]).
esListaString([H|T]):- string(H), esListaString(T).

%Predicado extra 2: verifica que la fecha tiene el formato escogido, es para evitar errores al crear nuevas preguntas y respuestas.
%Entra: [D,M,A]: lista de 3 enteros, (D:día/M:mes/A:año).
%Meta: un booleano.
esFecha([D, M, A]):- 
integer(D), integer(M), integer(A),
D > 0, D < 32, M > 0, M < 13.

%Predicado extra 3: verifica que la recompensa tenga el formato escogido, para evitar errores al crear preguntas.
%Entrada: [O,MT]: par de O: string (nombre ofertor), MT: entero (monto recompensa).
%Meta: una booleano.
esRecompensa([O,MT]):-
string(O), integer(MT), 
MT>0; ([O,MT]==["",0]).

%Predicado extra 4: permite representar una lista de strings como un solo string.
%Entrada: [H|T]: lista de string.
%Final: variable para el string resultante.
%Meta: un string.

listToString([],".").
listToString([H|T],Final):- 
atomics_to_string([" ",H, " -"], Str), listToString(T,Final2), string_concat(Str, Final2, Final).
%_________________________________________

%TDA usuario: representa un usuario y sus elementos obligatorios.
%Representación: [string nombre, string contraseña, entero reputación, lista de string referencias].

%Capa constructor: 
%Entrada: NU: un string (nombre del usuario), CU: un string (contraseña del usuario), RU: un entero mayor que 0 (reputación del usuario),
%LR: lista de strings (lista de referencias del usuario) y USUARIO: variable para asignar al usuario.
%Meta: un Usuario (lista de elementos) si las entradas son validas y un false sino.

crearUsuario(NU,CU,RU,LR, USUARIO):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1,
USUARIO= [NU,CU,RU,LR].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a un usuario, la lista posee las mismas entradas de Capa constructor.
%Meta: un booleano, un true si la lista es un usuario y un false sino.

esUsuario([]).
esUsuario([NU,CU,RU,LR]):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1 .
%_________________________________________
%TDA lista de usuarios: representa una lista de usuarios en el stack.
%Representación: [usuario1, usuario2, ......., usuarioN].

%Capa pertenencia:
%Entrada: [H|T]: una lista con elementos, sirve como postulante para una lista de usuarios.
%Meta: un booleano, un true si la lista corresponde a una lista de usuarios.
esListaUsuarios([]).
esListaUsuarios([H|T]):- esUsuario(H),esListaUsuarios(T).

%Como capa constructor se puede hacer un alcance de nombre con un predicado modificador, pues se puede empezar con una lista vacia de usuarios y luego ir agregando 
%los usuarios que se deseen: 
%El predicado agrega al final de la lista de usuarios un nuevo usuario.
%Entrada: [H|T]: una lista de usuarios o una nueva lista vacía, Usuario: un nuevo usuario, y [H|NEWT]: una variable para asignar la nueva lista.
%Meta: un booleano, un true si la lista corresponde a una lista de usuarios.

gregarUsuario([], Usuario, [Usuario]):- esUsuario(Usuario). %caso base, llega al final y agrega usuario.
agregarUsuario([H|T], Usuario, [H|NEWT]):- 
esListaUsuarios([H|T]), agregarUsuario(T, Usuario, NEWT). %avanza en la lista.

%Predicado extra:
%Permite determinara si un usuario con el nombre de entrada existe en una lista de usuarios.
%Entrada: [_|T]: lista de usuarios, NOMBRE: string (nombre de usuario buscado).
existeUsuario([[NOMBRE,_,_,_]|_], NOMBRE):- !. %si encuentra al usuario entrega true
existeUsuario([_|T], NOMBRE):- existeUsuario(T, NOMBRE). %avanza en la lista.

%_______
%Predicado extra usuarios:
%Permite transformar una lista de usuarios en un solo string ordenado.
%Entrada: 
%[[NameUser, Pass, Reputacion, Referencias]|Usuarios]: lista de usuarios.
%StringFinal: una variable para guardar el string resultante.
%Meta: string ordenado de lista de usuarios.

%Se llega al final de la lista, para.
listUserToString([],"\n\n"). 

%Recorre lista y va transformando usuarios en string y los va uniendo en un solo string con string_concat.
listUserToString([[NameUser, Pass, Reputacion, Referencias]|Usuarios], StringFinal):- esUsuario([NameUser, Pass, Reputacion, Referencias]),
listToString(Referencias,Ref),
atomics_to_string(["Nombre Usuario: ",NameUser, "\nClave: ",Pass, "\nReputacion: ",Reputacion, "\nReferencias: ",Ref, "\n\n"], Str),
listUserToString(Usuarios, StringFinal2), string_concat(Str, StringFinal2, StringFinal). 

%______
%Predicado extra, modificador.
%Predicado que actualiza la reputacion de un usuario. Encuentra a usuario segun su nombre.
%Entrada:
%[Usuario|Usuarios]: lista de usuarios.
%NameUser: string nombre de usuario.
%Monto: entero (monto que se quiere agregar a la reputación del usuario).
%[Usuario|NewUsuarios]: variable para la nueva lista de usuarios actualizada.
%Meta: una nueva lista  de usuarios con una recompensa modificada.

%Si se encuntra usuario, se modifica su reputacion y se detiene el predicado. 
actualizarRepUser([[NameUser,CU,R,LR]|Usuarios], NameUser, Monto, [[NameUser,CU,NewRep,LR]|Usuarios]):- NewRep is R + Monto.

%Sino se avanza en la lista.
actualizarRepUser([Usuario|Usuarios], NameUser, Monto, [Usuario|NewUsuarios]):- actualizarRepUser(Usuarios, NameUser, Monto, NewUsuarios).


%_________________________________________

%TDA respuesta: representa una respuesta y sus elementos obligatorios.
%Representación: [entero id respuesta, string autor, lista fecha de publicación, string contenido respuesta, lista etiquetas,
% string estado [Aceptada/Rechazada/Pendiente], entero votos a favor, entero votos en contra, entero reportes].

%Capa constructor:
%Entrada: IDR: entero (identificador respuesta), AR: string (autor de la respuesta), FP: lista de enteros (fecha de publicación), C: string (contenido), 
%LE: lista de strings (lista de etiquetas), EA: string (estado de aceptacion), VF: entero (votos a favor), VC: entero (votos en contra), 
%NR: entero (entero de reportes) y RESPUESTA: variable para asignar la respuesta contruida.
%Meta: un Respuesta (lista de elementos) si las entradas son correctas y un false sino.

crearRespuesta(IDR, AR, FP, C, LE, EA, VF, VC, NR, RESPUESTA):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR), 
IDR > -1, VF > -1, VC > -1, NR > -1,
RESPUESTA= [IDR, AR, FP, C, LE, EA, VF, VC, NR].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una respuesta, la lista posee las mismas entradas de Capa constructor.
%Meta: un booleano, un true si la lista es una respuesta y un false sino.

esRespuesta([]).
esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR),
IDR > -1, VF > -1, VC > -1, NR > -1 .
%_________________________________________
%TDA lista de respuestas: representa una lista de respuestas en una pregunta.
%Representación: [respuesta1, respuesta2, ......., respuestaN].

%Capa pertenencia:
%Entrada: [H|T]: una lista con elementos, sirve como postulante para una lista de respuestas.
%Meta: un booleano, un true si la lista corresponde a una lista de respuestas y un false sino.
esListaRespuestas([]).
esListaRespuestas([H|T]):- esRespuesta(H),esListaRespuestas(T).

%Capa constructor: se puede hacer un alcance de nombre con un predicado modificador, pues se puede empezar con una lista vacia de respuestas y luego ir agregando 
%las respuestas que se deseen: 
%Entrada: [H|T]: una lista de respuestas o una nueva lista vacia, Respuesta: la nueva respuesta y 
%[H|NEWT]: una variable para asignar la nueva lista de respuestas con la respuesta incluida.
%Meta: una nueva lista de respuestas.

agregarRespuesta([], Respuesta, [Respuesta]):- esRespuesta(Respuesta). %caso base, se llega al final de la lista y se agrega usuario.
agregarRespuesta([H|T], Respuesta, [H|NEWT]):- 
esListaRespuestas([H|T]), agregarRespuesta(T, Respuesta, NEWT). %avanza en la lista

%______
%Predicado extra de respuesta:
%Transforma una lista de respuestas en un string oredenado.
%Entrada: 
%[[IDR, AR, [D,M,A], C, Etiq, EA, VF, VC, NR]|Respuestas]: lista de respuestas, (IDR: id respuesta entero, AR: autor respuesta string,
%[D,M,A]: fecha(dia,mes,año), C: contenido string, Etiq: etiquetas lista string, EA: estado string, VF: votos + entero, VC: votos - entero, NR: reportes entero).
%StringRes: variable para asignar string resultante.

%Si llega al final para.
listResToString([], "\n\n").

%sino recorre la lista y va transformando respuestas en string y uniendolas con las otras respuestas con el predicado string_concat y atomics_to_string.
listResToString([[IDR, AR, [D,M,A], C, Etiq, EA, VF, VC, NR]|Respuestas], StringRes):- esRespuesta([IDR, AR, [D,M,A], C, Etiq, EA, VF, VC, NR]),
listToString(Etiq,E), atomics_to_string([D,"/",M,"/",A],FP),
atomics_to_string(["ID Respuesta: ",IDR, "\nAutor: ",AR, "\nFecha: ",FP , "\nContenido: ",C, "\nEtiquetas: ",E, "\nEstado: ",EA, "\nVotos a favor: ",VF, "\nVotos en contra: ",VC, "\nReportes: ",NR, "\n\n"], Str),
listResToString(Respuestas, StringRes2), string_concat(Str, StringRes2, StringRes).
%_______

%Predicado extra, modificador de lista.
%Predicado que recorre una lista de respuestas buscando una respuesta segun su id.
%Entrada: 
%[Respuesta|Respuestas]: una lista de respuestas.
%IDR: entero (identificador de la respuesta).
%AutorRes: variable que guardara un string (nombre autor respuesta).
%[Respuesta|NewRespuestas]: una variable para obtener la nueva lista de respuestas. 
%Meta: un booleano, una lista de respuestas actualizada y un string (nombre del autor de la respuesta).

%Si se encontro la respuesta segun su id, se modifica estado de pregunta, se obtiene el nombre del autor, el predicado se detiene. 
siExisteResAccept([[IDR, AutorRes, FP, C, LE, _, VF, VC, NR]|Respuestas], IDR, AutorRes, [[IDR, AutorRes, FP, C, LE, "Aceptada", VF, VC, NR]|Respuestas]):- !.

%Sino se avanza en la lista de respuestas.
siExisteResAccept([Respuesta|Respuestas], IDR, AutorRes, [Respuesta|NewRespuestas]):- siExisteResAccept(Respuestas, IDR, AutorRes, NewRespuestas).

%_________________________________________

%TDA pregunta: representa una preguntas y sus elementos obligatorios.
%Representación: [entero id pregunta, string nombre del autor, lista de enteros fecha de publicación, string contenido, lista de strings etiquetas,
%string estado pregunta [Abierta/Cerrada], entero número de visualizaciones, entero votos a favor, entero votos en contra, par Recompensa,
% entero reportes, lista de respuestas Respuestas].

%Capa constructor:
%Entrada: IDP: un entero (identificador de pregunta), AP: un string (nombre del autor de la pregunta), FP: lista de enteros (fecha de publicación de la pregunta), 
%C: un string (contenido pregunta), LE: una lista de strins (listas de etiquetas), EP: un string (estado de la pregunta), NV: entero (número de visualizaciones),
%VF: entero (votos a favor), VC: entero (votos en contra), REC: par (recompensa con ofertor y monto), RESPUESTAS: una lista de respuestas 
%(repuestas de la pregunta), PREGUNTA: una variable para asignar la pregunta construida. 
%Meta: una Pregunta (lista de elementos) si las entradas son correctas y un false sino. 

crearPregunta(IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS,PREGUNTA):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1,
PREGUNTA= [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una pregunta, la lista posee las mismas entradas de Capa constructor.
%Meta: un booleano, un true si la lista es una pregunta y un false sino.

esPregunta([]).
esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1 .

%_________________________________________
%TDA lista de preguntas: representa una lista de preguntas en el stack.
%Representación: [pregunta1, pregunta2, ......., preguntaN].

%Capa pertenencia:
%Entrada: [H|T]: una lista con elementos, sirve como postulante para una lista de preguntas.
%Meta: un booleano, un true si la lista corresponde a una lista de preguntas y un false sino.
esListaPreguntas([]).
esListaPreguntas([H|T]):- esPregunta(H),esListaPreguntas(T).
%______
%Capa constructor:se puede hacer un alcance de nombre con un predicado modificador, pues se puede empezar con una lista vacia de preguntas y luego ir agregando 
%las preguntas que se deseen: 
%Entrada: [H|T]: una lista de preguntas o una nueva lista vacia, Pregunta: la nueva pregunta y 
%[H|NEWT]: una variable para asignar la nueva lista de preguntas.
%Meta: una nueva lista de preguntas.

agregarPregunta([], Pregunta, [Pregunta]):- esPregunta(Pregunta). %caso base, se llega al final de la lista y se agrega usuario.
agregarPregunta([H|T], Pregunta,  [H|NEWT]):- 
esListaPreguntas([H|T]), agregarPregunta(T, Pregunta, NEWT).  %se avanza en la lista.

%______
%Predicado extra preguntas:
%Transforma una lista de preguntas en un string oredenado.
%Entrada: 
%[[IDP,AP,[D,M,A],C,LE,EP,NV,VF,VC,[Ofertor,Monto],NR,Respuestas]|Preguntas]: lista de preguntas, (IDP: id respuesta entero, AP: autor pregunta string,
%[D,M,A]: fecha(dia,mes,año), C: contenido string, LE: etiquetas lista string, EP: estado string, NV: visualizaciones entero, VF: votos + entero, VC: votos - entero, 
%[Ofertor, Monto] recompensa (string, entero), NR: reportes entero, Respuestas: lista de respuestas).
%StringPreg: variable para asignar string resultante.

%Si llega al final para.
listPregToString([], "\n").

%sino recorre la lista y va transformando preguntas en string y uniendolas con las otras preguntas con el predicado string_concat y atomics_to_string.

listPregToString([[IDP,AP,[D,M,A],C,LE,EP,NV,VF,VC,[Ofertor,Monto],NR,Respuestas]|Preguntas],StringPreg):- esPregunta([IDP,AP,[D,M,A],C,LE,EP,NV,VF,VC,[Ofertor,Monto],NR,Respuestas]),
listToString(LE,E), atomics_to_string([D,"/",M,"/",A],FP), atomics_to_string([Ofertor, " ofrece ", Monto, " puntos "],R), listResToString(Respuestas,Res),
atomics_to_string(["ID Pregunta: ",IDP, "\nAutor: ",AP, "\nFecha: ",FP, "\nContenido: ",C, "\nEtiquetas: ",E, "\nEstado: ",EP, "\nVisualizaciones: ",NV,"\nVotos a favor: ",VF, "\nVotos en contra: ",VC,"\nRecompensa: ",R, "\nReportes: ",NR,"\nRespuestas:\n\n",Res,"\n\n"], Str),
listPregToString(Preguntas, StringPreg2), string_concat(Str, StringPreg2, StringPreg).

%______
%Predicado extra 2 preguntas:
%Permite poner en una lista todas las preguntas que pertenecen a un usuario en especifico.
%Entrada: 
%[[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|Pregs]: lista de preguntas.
%NameUser: string (nombre del usuario del que se quieren reunir las preguntas).
%[[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|NewPreg]: variable para asignar la nueva lista de preguntas con todas las preguntas del usuario.
%Meta: lista de preguntas.

%Si se llega al final se detiene.
allPregUsuario([],_,[]).

%Si se encuentra una pregunta que tiene como autor al usuario, se guarda en la nueva lista.
allPregUsuario([[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|Pregs], NameUser, [[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|NewPreg]):- allPregUsuario(Pregs, NameUser, NewPreg).

%Si no se recorre la lista
allPregUsuario([_|Pregs], NameUser, NewPregs):- allPregUsuario(Pregs, NameUser, NewPregs).


%______
%Predicado estra 3, modificador.
%Predicado que busca una pregunta segun su id en la listas de preguntas en un stack, y cuando la encuentra verifica si se puede aceptar una de sus respuestas, identificada con 
%su id con el predicado siExisteResAccept, si esto pasa acepta la respuesta y obtiene el nombre de su autor, luego obtiene el monto de la recompensa si es que existe y modifica la
%pregunta quitando la recompensa, de esta forma se actualizan las respuestas y preguntas, y se obtiene monto de recompensa y autor de la respuesta.
%Entrada: 
%[Pregunta|Preguntas]: lista de preguntas.
%IDP: entero (identificador de la respuesta).
%AP: string (autor de la pregunta).
%IDR: entero (id de la respuesta).
%AutorRes: variable que guardara el nombre del autor de la respuesta.
%Monto: variable que guardara el monto de la recompensa si no hay recom es 0.
%[Pregunta|NewPreguntas]: una variable para una nueva lista de preguntas.
%Metas: una nueva lista de preguntas actualizada cuando se acepta una respuesta, un string nombre autor de la respuesta y un entero monto de la recompensa.
%Este predicado verifica además que el autor de la pregunta sea el usuario activo.

%Si se encontro la respuesta del id y autor correspondiente, se usa el predicado siExisteResAccept, se actualiza pregunta y se obtienen monto recompensa y autor respuesta. Se detiene con
%el operador de corte.
siExistePregDeUserActivoYResAccept([[IDP,AP,FP,C,LE,EP,NV,VF,VC,[_,Monto],NR,Respuestas]|Preguntas], IDP, AP, IDR, AutorRes, Monto, [[IDP,AP,FP,C,LE,EP,NV,VF,VC,["",0],NR,NRespuestas]|Preguntas]):-
siExisteResAccept(Respuestas, IDR, AutorRes, NRespuestas),  !.

%Si no se avanza en la lista. 
siExistePregDeUserActivoYResAccept([Pregunta|Preguntas], IDP, AP, IDR, AutorRes, Monto, [Pregunta|NewPreguntas]):- 
siExistePregDeUserActivoYResAccept(Preguntas, IDP, AP, IDR, AutorRes, Monto, NewPreguntas).


%_________________________________________

%TDA stack: representa a Stack Overflow.
%Representación: [usuario Sesión activa, lista de usuarios Registrados, lista de preguntas Preguntas stack, entero Correlativo pregunta, entero Correlativo respuestas].

%Capa constructor:
%Entrada: UA: un usuario (usuario activo en el stack), LU: una lista de usuarios (usuarios registrados en el stack), LP: una lista de preguntas (preguntas del stack),
% CP: un entero (correlativo para llevar la cuenta de la cantidad de preguntas en el stack), 
% CR: un entero (correlativo para llevar la cuenta de la cantidad de respuestas en el stack),
%STACK: variable para asignar el nuevo stack.
%Meta: un stack.

crearStack(UA, LU, LP, CP, CR, STACK):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1,
STACK= [UA, LU, LP, CP, CR].

%Capa pertenencia:
%Entrada: [UA, LU, LP, CP, CR]: un lista de elementos que sirven como postulante a ser un stack, la lista posee las mismas entradas de la capa constructor.
%Meta: un boolenao, un true si la lista es un stack y un false sino.

esStack([[],[],[],0,0]).
esStack([UA, LU, LP, CP, CR]):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1.


%predicados extras y modificadores para stack:

%Predicado que agrega un nuevo usuario a la lista de un stack mientras un usuario con el mismo nombre no exista. Lo agrega al final.
%Entradas: 
%[UA, [Usuario|Usuarios], LP, CP, CR]: stack.
%NewNameUser: un string (nombre del nuevo usuario que quiere registrarse).
%NewPass: la contraseña del nuevo usuario.
%[UA, NewUsuarios, LP, CP, CR]: una variable para asignar el nuevo stack.
%Meta: un stack con un nuevo usuario en la lista de usuarios.

%Cuando se llega al final de la lista de usuarios en el stack se agrega el nuevo usuario.
agregarUserStack([UA, [], LP, CP, CR], NameUser, Pass, [UA, [[NameUser, Pass, 0, []]], LP, CP, CR]):- string(NameUser),string(Pass).
%avanza en la lista.
agregarUserStack([UA, [Usuario|Usuarios], LP, CP, CR], NewNameUser, NewPass, [UA, [Usuario|NewUsuarios], LP, CP, CR]):-
agregarUserStack([UA, Usuarios, LP, CP, CR], NewNameUser, NewPass, [UA, NewUsuarios, LP, CP, CR]).

%______

%Predicado que autentifica a un usuario en el stack. Si su contraseña y nombre son correctos, se agrega a sesión activa.
%Entradas: 
%[UA, [Usuario|Usuarios], LP, CP, CR]: un stack.
%NameUser: string (nombre del usuario).
%Pass: string (contraséña usuario).
%[NewUA, [Usuario|NewUsuarios], LP, CP, CR]: una variable para asignar el nuevo stack con el usuario autentificado agregado en sesión activa.
%Meta: si el usuario es autentificado entrega un stack actualizado con sesión iniciada, si no se autentifica, entrega false.

%Si se encuentra usuario con ese nombre y contraseña, se agrega a sesión activa en el stack y el predicado termina.
autentificarUserEnStack([_, [[NameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR], NameUser, Pass, [[NameUser,Pass,Reput,Ref], [[NameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR]). 
%Se avanza en la lista de usuarios en el stack.
autentificarUserEnStack([UA, [Usuario|Usuarios], LP, CP, CR], NameUser, Pass, [NewUA, [Usuario|NewUsuarios], LP, CP, CR]):-
autentificarUserEnStack([UA, Usuarios, LP, CP, CR], NameUser, Pass, [NewUA, NewUsuarios, LP, CP, CR]).

%______

%Predicado que agrega una nueva pregunta en un stack con usuario en sesión activa.
%Entradas: 
%[[NombreUA,_,_,_], LU, [Pregunta|Preguntas], CP, CR]: stack con usuario en sesión activa.
%FechaP: lista de enteros (fecha de publicación).
%ContP: string (contenido pregunta).
%Etiq: lista de strings (etiquetas de la pregunta).
%[[], LU, [[CP, NombreUA, FechaP, ContP, Etiq,"Abierta",0,0,0,["",0],0,[]], Pregunta|Preguntas], NewCorrPreg, CR]: una variable que tomara el valor del stack
%actualizado con la nueva pregunta, el correlativo para preguntas aumentado en uno y el usuario activo eliminado de sesión activa.

%Caso en que el stack tenga una lista vacia de preguntas.
agregarAskStack([[NombreUA,_,_,_], LU, [], CP, CR], FechaP, ContP, Etiq, [[], LU, [[CP, NombreUA, FechaP, ContP, Etiq,"Abierta",0,0,0,["",0],0,[]]], NCP, CR]):- NCP is CP + 1.
%si el stack ya tiene preguntas.
agregarAskStack([[NombreUA,_,_,_], LU, [Pregunta|Preguntas], CP, CR], FechaP, ContP, Etiq, [[], LU, [[CP, NombreUA, FechaP, ContP, Etiq,"Abierta",0,0,0,["",0],0,[]], Pregunta|Preguntas], NCP, CR]):- NCP is CP + 1.

%En ambos casos si no existe sesión activa retorna false.

%______

%predicado para saber si existe una pregunta segun su id, en un stack con sesión activa (utiliza el operador de corte).
%Entrada: 
%[UA,LU,[_|Preguntas],CP,CR]: Stack con sesión activa.
%IDP: entero (identificador de la pregunta).
%Meta: un booleano.

%Si se encuentra pregunta y hay sesión activa entonces el operador de corte para la busqueda y se entrega true.
existePregEnStack([[_,_,_,_],_,[[IDP,_,_,_,_,_,_,_,_,_,_,_]|_],_,_], IDP):- !.
existePregEnStack([UA,LU,[_|Preguntas],CP,CR], IDP):- existePregEnStack([UA,LU,Preguntas,CP,CR], IDP). %Se avanza en la lista de preguntas del stack.

%______
%Predicado que permite agregar por adelante una respuestas a la lista de respuestas de una pregunta en un stack con usuario en sesión activa.
%Entrada: 
%[UA,LU,[Preg|Pregs],CP,CR]: un stack.
%IDP: un entero (identificador de la pregunta).
%Fecha: una representación de fecha (fecha de publicación de la respuesta).
%Cont: string (contenido de la respuesta).
%Etiq: una lista de string (etiquetas de la respuesta).
%[NewUA,LU,[Preg|NewPregs],CP,NCR]: un nuevo stack actualizado, sin sesion activa, con la respuesta realizada y el correlativo de respuestas aumentado.

%Caso de la pregunta no tuviera respuestas previas, se agrega la nueva respuestas a la lista vacia, se cierra sesión y aumenta correlativo.
agregarResPreg([[NUA,_,_,_],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[]]|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, [[],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[[CR,NUA,Fecha,Cont,Etiq,"",0,0,0]]]|Pregs],CP,NCR]):- NCR is CR + 1.

%caso de que existieran mas respuestas a la misma pregunta, se agrega la nueva respuesta a la lista de respuestas por adelante, se cierra sesión y se aumenta correlativo respuestas.
agregarResPreg([[NUA,_,_,_],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[Res|Resps]]|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, [[],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[[CR,NUA,Fecha,Cont,Etiq,"",0,0,0],Res|Resps]]|Pregs],CP,NCR]):- NCR is CR + 1.

%Se avanza en la lista de preguntas en el stack hasta encontrar la respuesta correspondiente al id.
agregarResPreg([UA,LU,[Preg|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, [NewUA,LU,[Preg|NewPregs],CP,NCR]):- agregarResPreg([UA,LU,Pregs,CP,CR], IDP, Fecha, Cont, Etiq, [NewUA,LU,NewPregs,CP,NCR]).


%_______________________________________
%Desarrollo requerimiento 2: Hechos.

usuario(["Maria", "Maria1999", 50, ["Racket","c#"]]).
usuario(["Ana","A1234", 70, ["java","python"]]).
usuario(["Juan","juan2000", 20, ["python","c++"]]).
usuario(["Pedro", "P340", 90,  ["java","c#"]]).

pregunta([0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]]).
pregunta([1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]]).
pregunta([2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]]).
pregunta([3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]]).
pregunta([4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]).

respuesta([2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0]).
respuesta([1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1]).
respuesta([0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]).
respuesta([3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0]).
respuesta([4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2]).
respuesta([5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0]).
respuesta([6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]).
respuesta([7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0]).
respuesta([8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]).
respuesta([9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]).

stack([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10]).

stack([[],[["Paola", "P1998", 53, ["Racket","c++"]],["Sam", "S123", 65, ["C#","python"]],["Pablo", "PO2001", 21, ["python","c"]],["Teo", "T342", 90, ["python","java"]]], 
			[
			[0, "Pablo", [28, 3, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 31, 12, 6, ["",0], 1, 
 			[
 			[2,"Sam", [12, 6, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 7, 3, 1],
 			[1,"Paola", [13, 4, 2020], "No es del todo una mala practica.", ["Variables globales"], "Rechazada", 6, 3, 0],
 			[0,"Teo", [4, 4, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Aceptada", 21, 2, 0]
 			]],
			[1, "Paola", [9, 9, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Sam", [3, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 25, 3, 0],
			[4, "Pablo", [20, 10, 2020], "No se puede hacer.", ["errores"], "Rechazada", 0, 15, 2],
 			[5, "Teo", [10, 10, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 2, 0]
 			]],
 			[2, "Pablo", [5, 11, 2020], "¿Como puedo hacer una lista de diferentes tipos de datos?", ["listas"], "Abierta", 20, 7, 13, ["Pedro",5], 0,
			[
			[6,"Teo", [7, 11, 2020], "Dependera del lenguaje.", ["construccion"], "Pendiente", 10, 2, 0],
			[7, "Paola", [6, 11, 2020], "No siempre se puede.", ["listas", "lenguaje"], "Pendiente", 9, 3, 0]
 			]],
 			[3, "Sam", [30, 11, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 28, 17, 1, ["Sam",20], 0, 
 			[]],
 			[4, "Teo", [2, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 36, 19, 1, ["",0], 0,
			[
			[8,"Paola", [3, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["Visualizacion prolog"], "Aceptada", 24, 0, 0],
 			[9, "Pablo", [2, 12, 2020], "Parece que no se puede.", ["Texto prolog"], "Rechazada", 0, 20, 3]
 			]]], 
 			5, 10]).

%_________________________________________

%Desarrollo requerimiento 3: predicado register.
%Predicado que agrega un nuevo usuario al stack si no existe usuario con ese nombre usando el predicado agregarUserStack.
%Entradas: 
%[UA,LU,LP,CP,CR]: un stack. (UA: usuario activo, LU: lista usuarios, LP: lista preguntas, CP: correlativo preg, CR: correlativo res).
%NewUserName: string (nombre nuevo usuario).
%PassUser: string (contraseña nuevo usuario).
%Stack2: variable para unificar o comparar stack actualizado.
%Meta: stack con nuevo usuario o booleano (false si existe el usuario ya, true si el Stack2 = stack actualizado, 
%y stack actualizado y true, en caso de registrar correctamente).
%El predicado verifica que se haya ingresado un stack, comprueba si existe usuario y agrega nuevo usuario en caso de no existir el nombre ya.

register([UA,LU,LP,CP,CR], NewUserName, PassUser, Stack2):-
esStack([UA,LU,LP,CP,CR]), not(existeUsuario(LU,NewUserName)),agregarUserStack([UA,LU,LP,CP,CR], NewUserName, PassUser, Stack2).

%Ejemplos:
/*
% 1 Se registra correctamente:
register([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Antonia", "ANTO123",SF).

% 2 no se registra usuario, pues existe nombre: entrega false.
register([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Ana", "AN123",SF).

% 3 se registra un usuario en un stack nuevo:
register([[],[],[],0,0], "Antonia", "ANTO123",SF).

*/

%_________________________________________

%Desarrollo requerimiento 4: predicado login.
%predicado que permite iniciar sesión a un usuario registrado.
%Entradas: 
%Stack: un stack.
%UserName: string (nombre usuario).
%Pass: string (contraseña usuario).
%Stack2: variable para unificar o comparar stack actualizado.
%Meta: stack con usuario activo o booleano (false si no se autentifica usuario, true si el Stack2 = stack actualizado, 
%y stack actualizado y true, en caso de iniciar sesión correctamente).
%El predicado verifica que se ingresa un stack y si el nombre y contraseña son datos correctos, luego autentifica usuario con el predicado autentificarUserEnStack.

login(Stack, UserName, Pass, Stack2):-
esStack(Stack), string(UserName), string(Pass),
autentificarUserEnStack(Stack, UserName, Pass, Stack2).

%Ejemplos:
/*
% 1 ingresa correctamente a usuario activo.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Ana", "A1234",SF).


%No inicia sesión. Contraseña incorrecta.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Ana", "A123",SF).

%No inicia sesión. No existe usuario.
login([[],[],[],0,0],"Ana", "A123",SF).
*/
%_________________________________________

%Desarrollo requerimiento 5: predicado ask.
%Predicado que permite realizar una pregunta a un usuario activo.
%Entradas: 
%Stack: un stack.
%FechaP: una representacion de fecha (fecha publicación).
%ContP: string (contenido de la pregunta).
%Etiq: una lista de strings (etiquetas de la pregunta).
%Stack2: variable para unificar o comparar stack actualizado.
%Meta: stack con nueva pregunta o booleano (false si no se puede realizar pregunta (no existe usuario activo o datos incorrectos), true si el Stack2 = stack actualizado, 
%y stack actualizado y true, en caso de realizar pregunta correctamente).
% El predicado verifica entradas y actualiza stack con el predicado agregarAskStack, agregando pregunta y eliminando sesión activa.

ask(Stack, FechaP, ContP, Etiq, Stack2):-
esStack(Stack), esFecha(FechaP), string(ContP), esListaString(Etiq),
agregarAskStack(Stack, FechaP, ContP, Etiq, Stack2).

%Ejemplos:
/*
% 1 realiza pregunta.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Ana", "A1234",SF), ask(SF, [02,10,2010], "esto funciona?", ["prueba","ask"], S).

% 2 no realiza pregunta: No existe usuario activo.
ask([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], [02,10,2010], "esto funciona?", ["prueba","ask"], S).

% 3 realiza pregunta.
ask([["Pedro", "P340", 90, ["java","c#"]],[["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]],[],0,0],
	 [02,10,2010], "esto funciona?", ["prueba","ask"], S).
*/

%_________________________________________

%Desarrollo requerimiento 7: predicado answer.
%Predicado que permite agregar una respuestas a una lista de respuestas de una pregunta en un stack con sesion activa.
%Entrada:
%[UA,LU,LP,CP,CR]: un stack.
%Fecha: una representación de fecha (fecha de publicación).
%IDP: un entero (id pregunta).
%Contenido: string (contenido respuesta).
%ListEtiq: lista de strings (etiquetas respuesta).
%Stack2: variable para unificar o comparar stack actualizado.
%Meta: stack con nueva respuesta o booleano (false si no se puede realizar la respuesta (no existe usuario activo o pregunta, o datos incorrectos), true si el Stack2 = stack actualizado, 
%y stack actualizado y true, en caso de realizar la respuesta correctamente).
%El predicado verifica los datos de entrada, comprueba que existe pregunta y usuario activo con existePregEnStack, y actualiza el stack con todos los cambios que conlleva agregar una respuesta
%con el predicado agregarResPreg.

answer([UA,LU,LP,CP,CR], Fecha, IDP, Contenido, ListEtiq, Stack2):-
esStack([UA,LU,LP,CP,CR]), esFecha(Fecha), integer(IDP), string(Contenido), esListaString(ListEtiq), 
existePregEnStack([UA,LU,LP,CP,CR],IDP), agregarResPreg([UA,LU,LP,CP,CR], IDP, Fecha, Contenido, ListEtiq, Stack2).


%Ejemplos answer:
/*
% 1 Realiza pregunta:
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]]], 
 			3, 9], "Ana", "A1234",SF),answer(SF, [2,2,2020], 2, "CONTENIDO RESPUESTA", ["prueba"], S2).


% 2 No realiza pregunta: No existe usuario activo:
answer([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], [2,2,2020], 2, "CONTENIDO RESPUESTA", ["prueba"], S2).

% 3 No realiza respuesta: No existe pregunta.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[3,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[4, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]]], 
 			3, 5], "Ana", "A1234",SF),answer(SF, [2,2,2020], 4, "CONTENIDO RESPUESTA", ["prueba"], S2).

*/
%_________________________________________

%Desarrollo requerimiento 8: predicado accept.
%Predicado que le permita a un usuario activo en el stack, aceptar una respuesta a una de susu preguntas.
%Entrada: 
%[[NomUserActivo,CUA,RUA,REP], ListUser, ListPreg, CorrPreg, CorrRes]: stack con usuario activo.
%IdPreg: entero( id de pregunta).
%IdRes: entero (id de respuesta).
%Stack2: variable para unificar o comparar stack actualizado.
%Meta: stack con respuesta aceptada o booleano (false si no se puede aceptar(no existe usuario activo o pregunta o respuesta, o datos incorrectos, o respuesta no perteneciente a usuario activo)
%, true si el Stack2 = stack actualizado, y stack actualizado y true, en caso de realizar la actualización correctamente).

%El predicado verifica los datos de entrada, realiza la actualización con el predicado siExistePregDeUserActivoYResAccept, si este entrega true, se puedo realizar el cambio y se modifican las
%las reputaciones correspondientes, por ultimo se actualiza stack. Si se entregaba false se acababa.

accept([[NomUserActivo,CUA,RUA,REP], ListUser, ListPreg, CorrPreg, CorrRes], IdPreg, IdRes, Stack2):-
esStack([[NomUserActivo,CUA,RUA,REP], ListUser, ListPreg, CorrPreg, CorrRes]), integer(IdPreg), integer(IdRes),
siExistePregDeUserActivoYResAccept(ListPreg, IdPreg, NomUserActivo, IdRes, AutorRes, MontoRecom, NewListPreg), M is MontoRecom + 15,
actualizarRepUser(ListUser, NomUserActivo, 2, ListUser2), actualizarRepUser(ListUser2, AutorRes, M, ListUser3),
Stack2 = [[], ListUser3, NewListPreg, CorrPreg, CorrRes].

%Ejemplos:
/*
% 1 acepta una respuesta. Entrega true y nuevo stack.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Pendiente", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Ana", "A1234",SF),accept(SF, 1, 5, F).

% 2 No acepta respuesta: la respuesta no corresponde a  la pregunta con el id. Entrega false.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Pendiente", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Ana", "A1234",SF), accept(SF, 0, 5, F).

%No acepta, la respuesta y pregunta exiten y se corresponden pero el autor de la pregunta no es el usuario activo. Entrega false.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Pendiente", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Maria", "Maria1999",SF), accept(SF, 1, 5, F).
*/
%_________________________________________

%Desarrollo requerimiento 9: predicado stackToString.
%Permite transformar un stack en un string ordenado.
%Entrada:
%[[NUA,CU,RU,REP], LU, LP, CP, CR]: un stack.
%StackStr: variable para unificar o comparar el string del stack.
%Meta: string del stack o booleano ( true si el Stack2 = stack actualizado sino false, 
%y string de stack y true, en caso de que StackStr sea variable libre).

%la funcion verifica que se entrege un stack.
%Si no existe usuario activo, se transforma todo el stack en string, usando los predicados: listUserToString, listPregToString, atomics_to_string.

stackToString([[], LU, LP, CP, CR], StackStr):- esStack([[], LU, LP, CP, CR]),
listUserToString(LU,LUStr), listPregToString(LP,LPStr), atomics_to_string(["StackOverflow:\n\n", "Usuarios:\n",LUStr, "Preguntas:\n",LPStr], StackStr).

%Si existe usuario activo se entrega un string del usuario activo, sus datos y preguntas. Usa los predicados: listUserToString, allPregUsuario, listPregToString y atomics_to_string.

stackToString([[NUA,CU,RU,REP], LU, LP, CP, CR], StackStr):- esStack([[NUA,CU,RU,REP], LU, LP, CP, CR]),
listUserToString([[NUA,CU,RU,REP]],UAStr),allPregUsuario(LP,NUA,NLP), listPregToString(NLP,LPStr),
atomics_to_string(["\nUsuario Activo:\n",UAStr, "Preguntas usuario:\n",LPStr], StackStr).

%Ejemplos:
/*
%Muestra todo el stack como un string.
stackToString([[],[["Paola", "P1998", 53, ["Racket","c++"]],["Sam", "S123", 65, ["C#","python"]],["Pablo", "PO2001", 21, ["python","c"]],["Teo", "T342", 90, ["python","java"]]], 
			[
			[0, "Pablo", [28, 3, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 31, 12, 6, ["",0], 1, 
 			[
 			[2,"Sam", [12, 6, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 7, 3, 1],
 			[1,"Paola", [13, 4, 2020], "No es del todo una mala practica.", ["Variables globales"], "Rechazada", 6, 3, 0],
 			[0,"Teo", [4, 4, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Aceptada", 21, 2, 0]
 			]],
			[1, "Paola", [9, 9, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Sam", [3, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 25, 3, 0],
			[4, "Pablo", [20, 10, 2020], "No se puede hacer.", ["errores"], "Rechazada", 0, 15, 2],
 			[5, "Teo", [10, 10, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 2, 0]
 			]],
 			[2, "Pablo", [5, 11, 2020], "¿Como puedo hacer una lista de diferentes tipos de datos?", ["listas"], "Abierta", 20, 7, 13, ["Pedro",5], 0,
			[
			[6,"Teo", [7, 11, 2020], "Dependera del lenguaje.", ["construccion"], "Pendiente", 10, 2, 0],
			[7, "Paola", [6, 11, 2020], "No siempre se puede.", ["listas", "lenguaje"], "Pendiente", 9, 3, 0]
 			]],
 			[3, "Sam", [30, 11, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 28, 17, 1, ["Sam",20], 0, 
 			[]],
 			[4, "Teo", [2, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 36, 19, 1, ["",0], 0,
			[
			[8,"Paola", [3, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["Visualizacion prolog"], "Aceptada", 24, 0, 0],
 			[9, "Pablo", [2, 12, 2020], "Parece que no se puede.", ["Texto prolog"], "Rechazada", 0, 20, 3]
 			]]], 
 			5, 10],ST), write(ST).

%Muestra al usuario activo y sus datos como un string.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Maria", "Maria1999",SF),stackToString(SF,ST),write(ST).

%Muestra todo el stack:
stackToString([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], ST),write(ST).
*/
%_______________________________________

%Desarrollo requerimiento 10: predicado vote.

%________
%Predicado que permite tomar una pregunta determinada por su id en el stack.
%Entrada: 
%[UA,LU,[[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]|_],CP,CR]: stack,
%IDP: entero (id de pregunta).
%[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]: una pregunta. (la pregunta buscada).

%Si encuentra la pregunta con el id, se entrega y termina el predicao.
getQuestion([_,_,[[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]|_],_,_], IDP, [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]):- !.
%Sino, se avanza en la lista de preguntas del stack.
getQuestion([UA,LU,[_|Preguntas],CP,CR], IDP, Pregunta):- getQuestion([UA,LU,Preguntas,CP,CR], IDP, Pregunta).

%________
%Predicado que permite tomar una respuesta de una lista de respuestas segun su id.
%Entrada: una lista de respuestas, IDR: entero (id de repuesta), y Respuesta: una variable para tomar la respuesta.
%Meta: una respuestas.

%Si se encontro la respuesta se entrega y se termina el predicado.
tomarRespuesta([[IDR,AR,FP,C,LE,EA,VF,VC,NR]|_], IDR, [IDR,AR,FP,C,LE,EA,VF,VC,NR]):- !.
%Sino se vanza en la lista.
tomarRespuesta([_|Respuestas], IDR, Respuesta):- tomarRespuesta(Respuestas, IDR, Respuesta).

%________
%Predicado que permite tomar una respuesta identificada por su id en un stack.
%Entrada: una stack, IDP: entero (id pregunta), IDR: entero (id respuesta), Respuesta: variable para tomar la respuesta.
%Meta: respuesta: 

%Si se encuentra pregunta se entrega.
getAnswer([_,_,[[IDP,_,_,_,_,_,_,_,_,_,_,Respuestas]|_],_,_], IDP, IDR, Respuesta):- tomarRespuesta(Respuestas, IDR, Respuesta).

%Sino se avanza en la lista de preguntas del stack.
getAnswer([UA,LU,[_|Preguntas],CP,CR], IDP, IDR, Respuesta):- getAnswer([UA,LU,Preguntas,CP,CR], IDP, IDR, Respuesta).

%________

%Intento vote respuesta.
%voteRes([[IDR,AR,FP,C,LE,EA,VF,VC,NR]|Resps], LU, [IDR,AR,FP,C,LE,EA,VF,VC,NR], true, [[IDR,AR,FP,C,LE,EA,NVF,NVC,NR]|Resps], NLU):-
%NVF is VF + 1, NVC is VC, actualizarRepUser(LU, AR, 10, NLU).
%voteRes([[IDR,AR,FP,C,LE,EA,VF,VC,NR]|Resps], LU, [IDR,AR,FP,C,LE,EA,VF,VC,NR], false, [[IDR,AR,FP,C,LE,EA,NVF,NVC,NR]|Resps], NLU):-
%NVC is VC + 1, NVF is VF, actualizarRepUser(LU, NUA, -2, NLU).
%voteRes([Res|Resps], LU, Respuesta, Booleano, [Res|NewResps], NLU):- voteRes(Resps, LU, Respuesta, Booleano, NewResps, NewLU).
%vote([[NUA,CU,RU,REP],LU,[[IDP,AP,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDR,AR,FPR,CR,LER,ER,VFR,VCR,NRR], Booleano, [[],NLU,[[IDP,AP,F,C,LE,E,NV,VF,VC,REC,NR,NewResps]|Pregs],CP,CR]):-
%voteRes(Resps, LU, [IDR,AR,FPR,CR,LER,ER,VFR,VCR,NRR], Booleano, NewResps, NLU), !.
%vote():-
%vote([[NUA,CU,RU,REP],LU,Pregs,CP,CR],[IDR,AR,FPR,CR,LER,ER,VFR,VCR,NRR], Booleano, [[],NewLU,Pregs,CP,CR]).


%Vote pregunta correcto:
%Predicado que realiza un voto a una pregunta o respuesta segun se entrege como entrada y puede ser voto + true o voto - false, debe haber usuario activo.
%Entrada: un stack, una respuesta o pregunta, un booleano y una variable para tomar el satck actualizado con el voto.
%Meta: stack actualizado con voto realizado.

%si se entrego una pregunta y un true, se vota a favor por la pregunta: 
%Cuando se encuentra la respuesta en la lista de respuestas del stack, se agrega el voto positivo, se actualiza la reputación del autor y se cierra sesión activa.
vote([[NUA,CU,RU,REP],LU,[[IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], true, [[],NLU,[[IDP,NUA,F,C,LE,E,NV,NVF,NVC,REC,NR,Resps]|Pregs],CP,CR]):- 
NVF is VF + 1, NVC is VC, actualizarRepUser(LU, NUA, 10, NLU).

%si se entrego una pregunta y un false, se vota en contra por la pregunta: 
%Cuando se encuentra la respuesta en la lista de respuestas del stack, se agrega el voto negativo, se actualiza la reputación del autor y se cierra sesión activa.
vote([[NUA,CU,RU,REP],LU,[[IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], false, [[],NLU,[[IDP,NUA,F,C,LE,E,NV,NVF,NVC,REC,NR,Resps]|Pregs],CP,CR]):- 
NVC is VC + 1, NVF is VF, actualizarRepUser(LU, NUA, -2, NLU).


%Si se entrega respuesta y un booleano, pero aun no se encuentra la respuesta, se avanza en la lista.
vote([UA,LU,[Preg|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], Booleano, [NewUA,NewLU,[Preg|NewPregs],CP,CR]):- 
vote([UA,LU,Pregs,CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], Booleano, [NewUA,NewLU,NewPregs,CP,CR]).


/*
%Ejemplo: agrega voto.
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Ana", "A1234",SF),getQuestion(SF,1,P),vote(SF,P,true,F).

%agrega voto:

login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Maria", "Maria1999",SF),getQuestion(SF,0,P),vote(SF,P,false,F).

%No existe pregunta, no agrega:
login([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10], "Maria", "Maria1999",SF),getQuestion(SF,10,P),vote(SF,P,false,F).

*/



