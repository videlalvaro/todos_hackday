// Taken from http://dense13.com/blog/2009/05/03/converting-string-to-slug-javascript/
function string_to_slug(str) {
  str = str.replace(/^\s+|\s+$/g, ''); // trim
  str = str.toLowerCase();

  // remove accents, swap ñ for n, etc
  var from = "àáäâèéëêìíïîòóöôùúüûñç·/_,:;";
  var to   = "aaaaeeeeiiiioooouuuunc------";
  for (var i=0, l=from.length ; i<l ; i++) {
    str = str.replace(new RegExp(from.charAt(i), 'g'), to.charAt(i));
  }

  str = str.replace(/[^a-z0-9 -]/g, '') // remove invalid chars
    .replace(/\s+/g, '-') // collapse whitespace and replace by -
    .replace(/-+/g, '-'); // collapse dashes

  return str;
}

Todos = Ember.Application.create();

Todos.Todo = Em.Object.extend({
    slug: null,
    title: null,
    isDone: false,

    isDoneChanged: function () {
        Todos.todosController.updateTodo(this);
    }.observes('isDone')
});

Todos.todosController = Em.ArrayProxy.create({
  content: [],

  createTodo: function(title) {
    var slug = string_to_slug(title).substring(0, 32);
    var todo = Todos.Todo.create({ title: title, slug: slug});
    var that = this;
    this._saveTodo(todo, function() {
        that.pushObject(todo);
    }, function() {
        console.log("save error");
    });
  },

  updateTodo: function(todo) {
      this._saveTodo(todo, function() {
          console.log("todo updated");
      }, function() {
          console.log("update failed");
      });
  },

  _saveTodo: function(todo, success, error) {
      $.ajax({
          type: 'POST',
          url: '/todos/' + todo.get('slug'),
          data: {'title': todo.get('title'), 'isDone': todo.get('isDone')},
          dataType: 'json',
          headers: {
              'content-type': 'application/json'
          },
          success: success,
          error: error
      });
  },

  clearCompletedTodos: function() {
    this.filterProperty('isDone', true).forEach(function (item) {
        var that = this;
        this._deleteTodo(item, function(){
            console.log('DELETE success');
            that.removeObject(item);
        }, function() {
           console.log('DELETE failed');
        });
    }, this);
  },

  _deleteTodo: function(todo, success, error) {
      $.ajax({
         type: 'DELETE',
         url: '/todos/' + todo.get('slug'),
         success: success,
         error: error
      });
  },

  remaining: function() {
    return this.filterProperty('isDone', false).get('length');
  }.property('@each.isDone'),

  allAreDone: function(key, value) {
    if (value !== undefined) {
      this.setEach('isDone', value);

      return value;
    } else {
      return !!this.get('length') && this.everyProperty('isDone', true);
    }
  }.property('@each.isDone'),

  loadTodos: function() {
    var self = this;
    $.ajax({
      url: '/todos',
      dataType: 'json',
      success: function(data) {
        var todos = data.todos.map(function(item) {
          return Todos.Todo.create(item);
        });

        self.set('content', todos);
      },

      error: function() {
        console.log("error while loading todos");
      }
    });
  },
});

Todos.todosController.loadTodos();

Todos.StatsView = Em.View.extend({
  remainingBinding: 'Todos.todosController.remaining',

  remainingString: function() {
    var remaining = this.get('remaining');
    return remaining + (remaining === 1 ? " item" : " items");
  }.property('remaining')
});

Todos.CreateTodoView = Em.TextField.extend({
  insertNewline: function() {
    var value = this.get('value');

    if (value) {
      Todos.todosController.createTodo(value);
      this.set('value', '');
    }
  }
});

